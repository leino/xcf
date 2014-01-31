{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.Image.XCF
       (
         parse, parseLayerAt
       )
       where

import Control.Applicative
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as CharString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text.Encoding
import Data.Word hiding (Word)
import Data.Binary.IEEE754
import Data.Binary.Get
import qualified Data.Attoparsec as Attoparsec
import qualified Data.Attoparsec.Binary as Attoparsec
import qualified Data.Attoparsec.Combinator as ParserCombinators
import Data.Int
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State

import Codec.Image.XCF.Represented
import Codec.Image.XCF.Data.Word
import qualified Codec.Image.XCF.Data as Data
import qualified Codec.Image.XCF.Data.ColorMode as ColorMode
import qualified Codec.Image.XCF.Data.Image as Image
import qualified Codec.Image.XCF.Data.Property as Property
import qualified Codec.Image.XCF.Data.Version as Version
import qualified Codec.Image.XCF.Data.TextLayerFlags as TextLayerFlags
import qualified Codec.Image.XCF.Data.Channel as Channel
import qualified Codec.Image.XCF.Data.ColorMap as ColorMap
import qualified Codec.Image.XCF.Data.FloatingSelection as FloatingSelection
import qualified Codec.Image.XCF.Data.Offset as Offset
import qualified Codec.Image.XCF.Data.Opacity as Opacity
import qualified Codec.Image.XCF.Data.Path as Path
import qualified Codec.Image.XCF.Data.Tattoo as Tattoo
import qualified Codec.Image.XCF.Data.UserUnit as UserUnit
import qualified Codec.Image.XCF.Data.Parasite as Parasite
import qualified Codec.Image.XCF.Data.Vectors as Vectors
import qualified Codec.Image.XCF.Data.Layer as Layer
import qualified Codec.Image.XCF.Data.Hierarchy as Hierarchy
import qualified Codec.Image.XCF.Data.Level as Level
import qualified Codec.Image.XCF.Data.CompressionIndicator as CompressionIndicator
import qualified Codec.Image.XCF.Data.Tile as Tile

import Prelude hiding (take)

newtype Size = Size Int64 deriving (Show, Eq)
newtype CheckedParser a = CheckedParser (StateT Size Attoparsec.Parser a)
                        deriving (Monad, Alternative, Applicative, Functor)

-- This type-class constitutes our (tiny) API of binary parsers.
-- It is specified here because we want to re-use some parsers as either
-- CheckedParser or plain Attoparsec parsers
class Parsing p where
  anyWord8 :: p Word8
  word8 :: Word8 -> p Word8
  take :: Int -> p ByteString.ByteString
  anyWord32be :: p Word32
  word32be :: Word32 -> p Word32

instance Parsing Attoparsec.Parser where
  word8 = Attoparsec.word8
  anyWord8 = Attoparsec.anyWord8
  take = Attoparsec.take
  anyWord32be = Attoparsec.anyWord32be
  word32be = Attoparsec.word32be

instance Parsing CheckedParser where
  word8 = makeCheckable (Size 1) . Attoparsec.word8
  anyWord8 = makeCheckable (Size 1) Attoparsec.anyWord8
  anyWord32be = makeCheckable (Size 4) Attoparsec.anyWord32be
  word32be = makeCheckable (Size $ fromIntegral 4) . Attoparsec.word32be
  take n = makeCheckable (Size $ fromIntegral n) (Attoparsec.take n)

type ErrorMessage = String

parse :: ByteString.ByteString -> Attoparsec.Result Image.Image
parse = Attoparsec.parse image

parseLayerAt :: Data.LayerPointer -> ByteString.ByteString -> Attoparsec.Result Layer.Layer
parseLayerAt (Data.LayerPointer offset) =
  Attoparsec.parse layer . ByteString.drop (fromIntegral offset)

count :: (Monad p, Parsing p) => Int -> p a -> p [a]
count 0 _ = return []
count n p = do
  x <- p
  xs <- count (n-1) p
  return (x:xs)

-- Construct a parser that keeps track of how much it consumes, and
-- fails immediately when it goes above the given limit size.
-- When the given checked parser is done, we also check that precisely all of the input was consumed.
parseChecked :: String -> Size -> CheckedParser a -> Attoparsec.Parser a
parseChecked ctx sz@(Size n) (CheckedParser cp) = do
  (x, Size nc) <- (runStateT cp sz) Attoparsec.<?> ctx
  unless (nc == 0) $ fail $ unwords ["checked parse only counsumed", show (n - nc),
                                     "out of", show n, "bytes"]
  return x

-- Now we provide a way of lifting certain parsers up to checked parsers
makeCheckable :: Size -> Attoparsec.Parser a -> CheckedParser a
makeCheckable sz p =
  CheckedParser (consume sz >> lift p)
  where
    consume sz@(Size n) = do
      Size nl <- get
      -- do checking
      unless (n <= nl) $ fail $ unwords ["need to consume", show n,
                                         "bytes, but only", show nl, "bytes left"]
      modify (\(Size m) -> Size $ m - n)

values :: (Bounded a, Enum a) => [a]
values = enumFromTo minBound maxBound

-- parser for a represented and enumerable thing (flags, etc...)
representedEnumerable :: (Represented r a, Parsing p, Alternative p, Monad p, Enum a, Bounded a) =>
                         (r -> p r) -> p a
representedEnumerable p = ParserCombinators.choice $ map (\v -> (p $ representation v) >> return v) values

anyUword :: (Parsing p, Functor p) => p UWord
anyUword = UWord <$> anyWord32be

uWord :: (Functor p, Parsing p) => UWord -> p UWord
uWord (UWord w)  = UWord <$> word32be w

anyWord :: (Parsing p, Functor p) => p Word
anyWord = Word <$> anyWord32be

anyInt :: (Parsing p, Functor p, Monad p) => p Int
anyInt = do
  Word w <- anyWord
  return $ fromIntegral w

word :: Word -> Attoparsec.Parser Word
word (Word w) = Word <$> Attoparsec.word32be w

anyString :: (Alternative p, Monad p, Parsing p) => p Text.Text
anyString = 
  (nonEmptyString <|> emptyString)
  where
    nonEmptyString = do
      nPlusOne <- fromIntegral <$> anyUword
      let n = nPlusOne-1
      t <- utf8 n
      word8 0
      return t
      where
        utf8 :: (Monad p, Functor p, Parsing p) => Int -> p Text.Text
        utf8 n =
          (decodeUtf8' <$> take n) >>= either (fail . show) return
    emptyString :: (Functor p, Monad p, Parsing p) => p Text.Text
    emptyString = uWord 0 >> (return $ Text.empty)

image :: Attoparsec.Parser Image.Image
image = do
  Attoparsec.string $ CharString.pack "gimp xcf "
  version <- (representedEnumerable Attoparsec.string) <|>
             ((CharString.unpack <$> Attoparsec.take 4) >>= fail . (++) "unknown version: ")
  Attoparsec.word8 0
  width <- fromIntegral <$> anyUword
  height <- fromIntegral <$> anyUword
  colorMode <- representedEnumerable uWord
  imageProperties <- Attoparsec.manyTill'
                     (Attoparsec.choice $ map propertyOfType Property.allImageTypes)
                     (propertyOfType Property.EndType)
  layerPointers <- Attoparsec.manyTill' (Data.LayerPointer <$> anyUword) (uWord 0)
  channelPointers <- Attoparsec.manyTill' (Data.ChannelPointer <$> anyUword) (uWord 0)
  return $ Image.Image {
    Image.version = version,
    Image.colorMode = colorMode,
    Image.width = width,
    Image.height = height,
    Image.imageProperties = imageProperties,
    Image.layerPointers = layerPointers,
    Image.channelPointers = channelPointers    
    }

layer :: Attoparsec.Parser Layer.Layer
layer = do
  width <- fromIntegral <$> anyUword
  height <- fromIntegral <$> anyUword
  layerType <- representedEnumerable uWord
  name <- anyString
  properties <- Attoparsec.manyTill'
                (Attoparsec.choice $ map propertyOfType Property.allLayerTypes)
                (propertyOfType Property.EndType)
  hierarchyPointer <- Layer.HierarchyPointer <$> anyUword
  layerMaskPointer <- Layer.LayerMaskPointer <$> anyUword
  return $ Layer.Layer {
    Layer.width = width,
    Layer.height = height,
    Layer.layerType = layerType,
    Layer.name = name,
    Layer.properties = properties,
    Layer.hierarchyPointer = hierarchyPointer,
    Layer.layerMaskPointer = layerMaskPointer
    }

hierarchy :: Attoparsec.Parser Hierarchy.Hierarchy
hierarchy = do
  width <- fromIntegral <$> anyUword
  height <- fromIntegral <$> anyUword
  bytesPerPixel <- fromIntegral <$> anyUword
  levelPointer <- Hierarchy.LevelPointer <$> anyUword
  Attoparsec.many' $ satisfying anyUword ((/=) 0) -- bunch of level pointers which are unused
  uWord 0
  return $ Hierarchy.Hierarchy {
    Hierarchy.width = width,
    Hierarchy.height = height,
    Hierarchy.bytesPerPixel = bytesPerPixel,
    Hierarchy.levelPointer = levelPointer
    }

level :: Attoparsec.Parser Level.Level
level = do
  width <- fromIntegral <$> anyUword
  height <- fromIntegral <$> anyUword
  tilesPointer <- Level.TilesPointer <$> anyUword
  uWord 0
  return $ Level.Level {
    Level.width = width,
    Level.height = height,
    Level.tilesPointer = tilesPointer
    }

runs :: Int -> Attoparsec.Parser [Tile.Run]
runs 0 = return []
runs numBytesLeft = do
  n <- anyWord8
  let getLongRunlength = do
        p <- fromIntegral <$> anyWord8
        q <- fromIntegral <$> anyWord8
        return $ p*256 + q
  let modeParser
        | n <= 126 = do
          let runLength = (fromIntegral n) + 1
          when (runLength > numBytesLeft) $ fail $ messageNoMoreBytes runLength numBytesLeft
          v <- anyWord8
          return (runLength, Tile.Run runLength v)
        | n == 127 = do
          runLength <- getLongRunlength
          when (runLength > numBytesLeft) $ fail $ messageNoMoreBytes runLength numBytesLeft
          v <- anyWord8
          return (runLength, Tile.Run runLength v)
        | n == 128 = do
          runLength <- getLongRunlength
          when (runLength > numBytesLeft) $ fail $ messageNoMoreBytes runLength numBytesLeft
          bs <- take runLength
          return (runLength, Tile.Block bs)
        | n >= 129 = do
          let runLength = 256 - (fromIntegral n)
          when (runLength > numBytesLeft) $ fail $ messageNoMoreBytes runLength numBytesLeft
          bs <- take runLength
          return (runLength, Tile.Block bs)
        where
          messageNoMoreBytes runLength numBytesLeft =
            unwords ["found a run of length", show runLength, ", but have only",
                     show numBytesLeft, "bytes left to account for"]
  (runLength, r) <- modeParser
  ((:) r) <$> (runs $ numBytesLeft - runLength)

-- Generate a sequence of tile sizes for a level.
-- The sequence is in row-major, top-to-bottom and left-to-right order.
tileSizes :: Int -> Int -> [(Int, Int)]
tileSizes levelWidth levelHeight =
  (,) <$> (chunks levelWidth) <*> (chunks levelHeight)
  where
    -- make n in chunks of 64, plus possibly a smaller remainder chunk
    chunks n = case divMod n 64 of (q,0) -> replicate q 64
                                   (q,r) -> replicate q 64 ++ [r]
    
-- helper functions to read tiles of various 1,2,3,4 bytes per pixel
tile1bpp (w,h) = runs (w*h)
tile2bpp (w,h) = (,) <$> runs (w*h) <*> runs (w*h)
tile3bpp (w,h) = (,,) <$> runs (w*h) <*> runs (w*h) <*> runs (w*h)
tile4bpp (w,h) = (,,,) <$> runs (w*h) <*> runs (w*h) <*> runs (w*h) <*> runs (w*h)

tiles :: Int -> Int -> CompressionIndicator.CompressionIndicator ->
         ColorMode.ColorMode -> Attoparsec.Parser Tile.Tiles
tiles levelWidth levelHeight CompressionIndicator.None colorMode = do -- uncompressed raw tiles
  Tile.RawTiles <$> (Attoparsec.take $ levelWidth * levelHeight * ColorMode.bytesPerPixel colorMode)
tiles levelWidth levelHeight CompressionIndicator.RLE (ColorMode.NoAlpha ColorMode.RGB) = do
  Tile.RGBTiles <$> (mapM tile3bpp $ tileSizes levelWidth levelHeight)
tiles levelWidth levelHeight CompressionIndicator.RLE (ColorMode.Alpha ColorMode.RGB) = do
  Tile.RGBAlphaTiles <$> (mapM tile4bpp $ tileSizes levelWidth levelHeight)
tiles levelWidth levelHeight CompressionIndicator.RLE (ColorMode.NoAlpha ColorMode.GrayScale) = do
  Tile.GrayscaleTiles <$> (mapM tile1bpp $ tileSizes levelWidth levelHeight)
tiles levelWidth levelHeight CompressionIndicator.RLE (ColorMode.Alpha ColorMode.GrayScale) = do
  Tile.GrayscaleAlphaTiles <$> (mapM tile2bpp $ tileSizes levelWidth levelHeight)
tiles levelWidth levelHeight CompressionIndicator.RLE (ColorMode.NoAlpha ColorMode.Indexed) = do
  Tile.IndexedTiles <$> (mapM tile1bpp $ tileSizes levelWidth levelHeight)
tiles levelWidth levelHeight CompressionIndicator.RLE (ColorMode.Alpha ColorMode.Indexed) = do
  Tile.IndexedAlphaTiles <$> (mapM tile2bpp $ tileSizes levelWidth levelHeight)
  
anyUnit :: Attoparsec.Parser Property.Unit
anyUnit = representedEnumerable uWord

satisfying :: (Monad p, Parsing p) => p a -> (a -> Bool) -> p a
satisfying p cond = do
  x <- p
  if cond x then return x else fail "condition could not be satisfied"
  
divisible :: Integral a => a -> a -> Bool
divisible n x = x `mod` n == 0

anyFloat :: (Monad p, Parsing p) => p Float
anyFloat = do
  bs <- take 4
  return $ runGet getFloat32be $ LazyByteString.fromChunks [bs]

anyTattoo :: (Functor p, Parsing p) => p Tattoo.Tattoo
anyTattoo = Tattoo.Tattoo <$> anyUword

anyParasite :: (Monad p, Parsing p, Alternative p) => p Parasite.Parasite
anyParasite = do
  s <- anyString
  f <- anyUword
  n <- fromIntegral <$> anyUword
  bs <- take n
  return $ Parasite.Parasite {
    Parasite.name = s,
    Parasite.flags = f,
    Parasite.payload = bs
    }

propertyOfType :: Property.Type -> Attoparsec.Parser Property.Property
propertyOfType t = do
  uWord $ Property.representation t
  case t of
    Property.EndType -> uWord 0 >> return Property.EndProperty
    Property.ColorMapType -> do
      threeNPlus4 <- satisfying (fromIntegral <$> anyUword) (\n -> (n `mod` 3 == 1) && n >= 4)
      let numColors = (threeNPlus4 - 4) `div` 3
      numColorsCheck <- (fromIntegral <$> anyUword)
      unless (numColors == numColorsCheck) $ fail $
        unwords ["number of colors indicated by payload size was",
                 show numColors,
                 "but the number of colors indicated by the control word was",
                 show numColorsCheck]
      (Property.ColorMapProperty . ColorMap.ColorMap) <$>
        (count numColors $ ColorMap.Color <$> anyWord8 <*> anyWord8 <*> anyWord8)
    Property.ActiveLayerType -> uWord 0 >> return Property.ActiveLayerProperty
    Property.ActiveChannelType -> uWord 0 >> return Property.ActiveChannelProperty
    Property.SelectionType -> uWord 4 >> return Property.SelectionProperty
    Property.FloatingSelectionType -> 
      uWord 4 >> (Property.FloatingSelectionProperty . FloatingSelection.FloatingSelection) <$> anyUword
    Property.OpacityType ->
      uWord 4 >> (Property.OpacityProperty . Opacity.Opacity . fromIntegral) <$>
      (satisfying anyWord (\n -> n >= 0 && n < 256))
    Property.ModeType -> uWord 4 >> Property.ModeProperty <$> representedEnumerable uWord
    Property.VisibleType ->
      uWord 4 >> Property.VisibleProperty <$> ( (uWord 1 >> return True) <|> (uWord 0 >> return False) )
    Property.LinkedType ->
      uWord 4 >> Property.LinkedProperty <$> ( (uWord 1 >> return True) <|> (uWord 0 >> return False) )
    Property.LockContentType ->
      uWord 4 >> Property.LockContentProperty <$> ( (uWord 1 >> return True) <|> (uWord 0 >> return False) )
    Property.LockAlphaType -> 
      uWord 4 >> Property.LockAlphaProperty <$> ( (uWord 1 >> return True) <|> (uWord 0 >> return False) )
    Property.ApplyMaskType -> 
      uWord 4 >> Property.ApplyMaskProperty <$> ( (uWord 1 >> return True) <|> (uWord 0 >> return False) )
    Property.EditMaskType -> 
      uWord 4 >> Property.EditMaskProperty <$> ( (uWord 1 >> return True) <|> (uWord 0 >> return False) )
    Property.ShowMaskType -> 
      uWord 4 >> Property.ShowMaskProperty <$> ( (uWord 1 >> return True) <|> (uWord 0 >> return False) )
    Property.ShowMaskedType -> undefined
    Property.OffsetsType -> do
      uWord 8
      dx <- fromIntegral <$> anyWord
      dy <- fromIntegral <$> anyWord
      return $ Property.OffsetsProperty $ Offset.Offset {
        Offset.xOffset = dx,
        Offset.yOffset = dy
        }
    Property.ColorType -> 
      uWord 3 >> Property.ColorProperty <$> (Channel.Color <$> anyWord8 <*> anyWord8 <*> anyWord8)
    Property.CompressionType -> uWord 1 >> Property.CompressionProperty <$> representedEnumerable word8
    Property.GuidesType -> do
      n <- (flip div 5 . fromIntegral) <$> satisfying anyUword (divisible 5)
      Property.GuidesProperty <$>
        Attoparsec.count n
        (Property.Guide <$> (Property.GuideCoordinate <$> anyWord) <*> (representedEnumerable word8))
    Property.ResolutionType -> do
      uWord 8
      x <- satisfying anyFloat (> 0)
      y <- satisfying anyFloat (> 0)
      return $ Property.ResolutionProperty {
        Property.horizontalResolution = x,
        Property.verticalResolution = y
        }
    Property.TattooType -> uWord 4 >> Property.TattooProperty <$> anyTattoo
    Property.ParasitesType -> do
      l <- fromIntegral <$> anyUword
      -- use "checked" versions of all parsers, which keep track of how much has been parsed
      -- and fail immediately if we go over the limit, l
      ps <- parseChecked "parasites" (Size l) (many anyParasite)
      return $ Property.ParasitesProperty ps
    Property.UnitType -> uWord 4 >> Property.UnitProperty <$> anyUnit
    Property.PathsType -> do
      payloadSize <- (Size . fromIntegral) <$> anyUword
      let path :: CheckedParser Path.Path
          path = do
            name <- anyString
            locked <- ((/=) 0 . fromIntegral) <$> anyUword
            state <- anyWord8
            closed <- ((/=) 0 . fromIntegral) <$> anyUword
            -- check for inconsistent closedness/state combinations
            when (closed && state /= 4) $ do
              fail $ unwords $ ["found a closed curve with state ", show state,
                                "(closed curves should have state equal to 4)"]
            when ((not closed) && state /= 2) $ do
              fail $ unwords $ ["found a non-closed curve with state ", show state,
                                "(non-closed curves should have state equal to 2)"]
            numPoints <- fromIntegral <$> anyUword
            version <- representedEnumerable word8
            -- parse a dummy if not the integral path version
            when (version /= Path.Integral) $ uWord 1 >> return ()
            let anyPoint p = do
                  pointType <- representedEnumerable uWord
                  x <- p
                  y <- p
                  return $ Path.Point pointType x y
            case version of
              Path.Integral -> Path.IntegralPath <$> count numPoints (anyPoint anyInt)
              Path.Float -> Path.FloatPath <$> count numPoints (anyPoint anyFloat)
              Path.Tattoo -> do
                maybeTattoo <- (uWord 0 >> return Nothing) <|> (Just <$> anyTattoo)
                Path.TattooPath <$> count numPoints (anyPoint anyFloat)
      (activePathIdx, paths) <- parseChecked "paths" payloadSize $ do
        activePathIdx <- fromIntegral <$> anyUword
        numPaths <- fromIntegral <$> anyUword
        paths <- count numPaths path
        return (activePathIdx, paths)
      return $ Property.PathsProperty $ Path.Paths {Path.activeIdx = activePathIdx,
                                                    Path.paths = paths}
    Property.UserUnitType -> do
      payloadSize <- (Size . fromIntegral) <$> anyUword
      let userUnit = do
            factor <- satisfying anyFloat (>= 0.0)
            digits <- fromIntegral <$> anyUword
            id <- anyString
            symbol <- anyString
            abbreviation <- anyString
            nameSingular <- anyString
            namePlural <- anyString
            return $ UserUnit.UserUnit {
              UserUnit.factor = factor,
              UserUnit.numDecimals = digits,
              UserUnit.identifier = id,
              UserUnit.symbol = symbol,
              UserUnit.abbreviation = abbreviation,
              UserUnit.nameSingular = nameSingular,
              UserUnit.namePlural = namePlural
              }
      Property.UserUnitProperty <$> parseChecked "user unit" payloadSize userUnit
    Property.VectorsType -> do
      payloadSize <- (Size . fromIntegral) <$> anyUword
      parseChecked "vectors" payloadSize $ do
        uWord 1
        activePathIdx <- fromIntegral <$> anyUword
        numPaths <- fromIntegral <$> anyUword
        let path = do
              name <- anyString
              maybeTattoo <- (uWord 0 >> return Nothing) <|> (Just <$> anyTattoo)
              visible <- ((/=) 0 . fromIntegral) <$> anyUword
              linked <- ((/=) 0 . fromIntegral) <$> anyUword
              numParasites <- fromIntegral <$> anyUword
              numStrokes <- fromIntegral <$> anyUword
              parasites <- count numParasites anyParasite
              let anyStroke = do
                    uWord 1
                    closed <- ((/=) 0 . fromIntegral) <$> anyUword
                    numFloats <- satisfying anyUword (\n -> n >= 2 && n <= 6)
                    numControlPoints <- fromIntegral <$> anyUword
                    let anyControlPoint = do
                          t <- representedEnumerable uWord
                          x <- anyFloat
                          y <- anyFloat
                          pressure <- if numFloats >= 3 then anyFloat else return 1.0
                          xTilt <- if numFloats >= 4 then anyFloat else return 0.5
                          yTilt <- if numFloats >= 5 then anyFloat else return 0.5
                          wheel <- if numFloats == 6 then anyFloat else return 0.5
                          return $ Vectors.ControlPoint {
                            Vectors.pointType = t,
                            Vectors.x = x,
                            Vectors.y = y,
                            Vectors.pressure = pressure,
                            Vectors.xTilt = xTilt,
                            Vectors.yTilt = yTilt,
                            Vectors.wheel = wheel
                            }
                    points <- count numControlPoints anyControlPoint
                    return $ Vectors.Stroke {Vectors.closed = closed,
                                             Vectors.controlPoints = points}
              strokes <- count numStrokes anyStroke
              return $ Vectors.Path {
                Vectors.name = name,
                Vectors.tattoo = maybeTattoo,
                Vectors.visible = visible,
                Vectors.linked = linked,
                Vectors.parasites = parasites,
                Vectors.strokes = strokes}
        paths <- count numPaths path
        return $ Property.VectorsProperty $ Vectors.Vectors {
          Vectors.activePathIdx = activePathIdx,
          Vectors.paths = paths
          }
    Property.TextLayerFlagsType -> 
      uWord 4 >> (Property.TextLayerFlagsProperty . TextLayerFlags.TextLayerFlags) <$> anyUword
    Property.SamplePointsType -> undefined
    Property.GroupItemType -> undefined
    Property.ItemPathType -> undefined
    Property.GroupItemFlagsType -> undefined