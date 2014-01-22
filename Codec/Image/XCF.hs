{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.Image.XCF where

import Control.Applicative
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as CharString
import Data.Text.Encoding
import Data.Word hiding (Word)
import qualified Codec.Image.XCF.Data.ColorMode as ColorMode
import qualified Codec.Image.XCF.Data.Image as Image
import qualified Codec.Image.XCF.Data.Property as Property
import qualified Codec.Image.XCF.Data.Version as Version
import qualified Codec.Image.XCF.Data.TextLayerFlags as TextLayerFlags

import Codec.Image.XCF.Represented
import Codec.Image.XCF.Data.Word
import Codec.Image.XCF.Data.Path as Path
import Codec.Image.XCF.Data.Tattoo as Tattoo
import Codec.Image.XCF.Data.UserUnit as UserUnit
import Codec.Image.XCF.Data.Parasite as Parasite
import Codec.Image.XCF.Data.Vectors as Vectors

import qualified Data.Attoparsec as Attoparsec
import qualified Data.Attoparsec.Binary as Attoparsec
import qualified Data.Attoparsec.Combinator as ParserCombinators
import Data.Int
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Prelude hiding (take)

data Color = Color RGB |
             GrayscaleColor Intensity |
             IndexedColor ColorMapIndex
data RGB = RGB Intensity Intensity Intensity
newtype Intensity = Intensity Word8
newtype ColorMapIndex = ColorMapIndex Word8
newtype ColorMap = ColorMap (ColorMapIndex -> RGB)
newtype Alpha = Alpha Intensity
data Pixel = Pixel Alpha Color
newtype ParasiteIdentifier = ParasiteIdentifier Text.Text
data Parasite = Parasite ParasiteIdentifier
newtype LayerPointer = LayerPointer UWord
newtype ChannelPointer = ChannelPointer UWord

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
      n <- fromIntegral <$> anyWord8
      t <- utf8 n
      word8 0
      return t
      where
        utf8 :: (Monad p, Functor p, Parsing p) => Int -> p Text.Text
        utf8 n =
          (decodeUtf8' <$> take n) >>= either (fail . show) return
    emptyString :: (Monad p, Parsing p) => p Text.Text
    emptyString = word8 0 >> (return $ Text.empty)

version :: Version.Version -> Attoparsec.Parser Version.Version
version v = do
  Attoparsec.string $ representation v
  return v
  
anyVersion :: Attoparsec.Parser Version.Version
anyVersion = do
  let allVersions = values :: [Version.Version]
  (Attoparsec.choice $ map version allVersions) <|>
    ((CharString.unpack <$> Attoparsec.take 4) >>= fail . (++) "unknown version: ") -- tack on a fail parser

colorMode :: ColorMode.ColorMode -> Attoparsec.Parser ColorMode.ColorMode
colorMode m = do
  uWord $ ColorMode.representation m
  return m
  
anyColorMode :: Attoparsec.Parser ColorMode.ColorMode
anyColorMode = do
  let allModes = enumFromTo minBound maxBound :: [ColorMode.ColorMode]
  Attoparsec.choice $ map colorMode allModes

anyLayerPointer :: Attoparsec.Parser LayerPointer
anyLayerPointer = LayerPointer <$> anyUword

anyChannelPointer :: Attoparsec.Parser ChannelPointer
anyChannelPointer = ChannelPointer <$> anyUword

anyImageProperty = undefined

image :: Attoparsec.Parser Image.Image
image = do
  Attoparsec.string $ CharString.pack "gimp xcf "
  v <- anyVersion
  Attoparsec.word8 0
  width <- anyUword
  height <- anyUword
  mode <- anyColorMode
  imgprops <- Attoparsec.many' anyImageProperty
  layerptrs <- Attoparsec.many' anyLayerPointer
  Attoparsec.word8 0
  channelptrs <- Attoparsec.many' anyChannelPointer
  Attoparsec.word8 0
  return undefined
  
propertyType :: Property.Type -> Attoparsec.Parser Property.Type
propertyType t = do
  uWord $ Property.representation t
  return t
  
compressionIndicator :: Property.CompressionIndicator -> Attoparsec.Parser Property.CompressionIndicator
compressionIndicator i = Attoparsec.word8 (Property.representation i) >> return i

guideOrientation :: Property.GuideOrientation -> Attoparsec.Parser Property.GuideOrientation
guideOrientation o = Attoparsec.word8 (Property.representation o) >> return o

unit :: Property.Unit -> Attoparsec.Parser Property.Unit
unit u = uWord (Property.representation u) >> return u

anyCompressionIndicator :: Attoparsec.Parser Property.CompressionIndicator
anyCompressionIndicator = Attoparsec.choice $ map compressionIndicator values

anyGuideOrientation :: Attoparsec.Parser Property.GuideOrientation
anyGuideOrientation = Attoparsec.choice $ map guideOrientation values

anyUnit :: Attoparsec.Parser Property.Unit
anyUnit = Attoparsec.choice $ map unit values

anyGuide :: Attoparsec.Parser Property.Guide
anyGuide = do
  c <- (Property.GuideCoordinate <$> anyWord)
  o <- anyGuideOrientation
  return $ Property.Guide c o
  
satisfying :: (Monad p, Parsing p) => p a -> (a -> Bool) -> p a
satisfying p cond = do
  x <- p
  if cond x then return x else fail "condition could not be satisfied"
  
divisible :: Integral a => a -> a -> Bool
divisible n x = x `mod` n == 0

anyFloat :: Parsing p => p Float
anyFloat = undefined

anyTattoo :: (Functor p, Parsing p) => p Tattoo.Tattoo
anyTattoo = Tattoo.Tattoo <$> anyUword

anyParasite :: (Monad p, Parsing p, Alternative p) => p Parasite.Parasite
anyParasite = do
  s <- anyString
  f <- anyWord8
  n <- fromIntegral <$> anyWord8
  bs <- take n
  return $ Parasite.Parasite {
    Parasite.name = s,
    Parasite.flags = f,
    Parasite.payload = bs
    }


propertyOfType :: Property.Type -> Attoparsec.Parser Property.Property
propertyOfType t = do
  propertyType t
  case t of
    Property.EndType -> undefined
    Property.ColorMapType -> undefined
    Property.ActiveLayerType -> undefined
    Property.ActiveChannelType -> undefined
    Property.SelectionType -> undefined
    Property.FloatingSelectionType -> undefined
    Property.OpacityType -> undefined
    Property.ModeType -> undefined
    Property.VisibleType -> undefined
    Property.LinkedType -> undefined
    Property.LockAlphaType -> undefined
    Property.ApplyMaskType -> undefined
    Property.EditMaskType -> undefined
    Property.ShowMaskType -> undefined
    Property.ShowMaskedType -> undefined
    Property.OffsetsType -> undefined
    Property.ColorType -> undefined
    Property.CompressionType -> do
      uWord $ fromIntegral 1
      Property.CompressionProperty <$> anyCompressionIndicator
    Property.GuidesType -> do
      n <- (flip div 5) <$> satisfying (fromIntegral <$> anyUword) (divisible 5)
      Property.GuidesProperty <$> Attoparsec.count n anyGuide
    Property.ResolutionType -> do
      satisfying (fromIntegral <$> anyUword) ((==) 8)
      x <- satisfying anyFloat (> 0)
      y <- satisfying anyFloat (> 0)
      return $ Property.ResolutionProperty {
        Property.horizontalResolution = x,
        Property.verticalResolution = y
        }
    Property.TattooType -> do
      satisfying (fromIntegral <$> anyUword) ((==) 4)
      Property.UnitProperty <$> anyUnit
    Property.ParasitesType -> do
      l <- fromIntegral <$> anyWord8
      -- use "checked" versions of all parsers, which keep track of how much has been parsed
      -- and fail immediately if we go over the limit, l
      ps <- parseChecked "parasites" (Size l) (many anyParasite)
      return $ Property.ParasitesProperty ps
    Property.UnitType -> satisfying (fromIntegral <$> anyUword) ((==) 4) >> Property.UnitProperty <$> anyUnit
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
            when (version /= Path.Integral) $ satisfying (fromIntegral <$> anyUword) ((==) 1) >> return ()
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
            return $ UserUnit.UserUnit {UserUnit.factor = factor,
                                        UserUnit.numDecimals = digits,
                                        UserUnit.identifier = id,
                                        UserUnit.symbol = symbol,
                                        UserUnit.abbreviation = abbreviation,
                                        UserUnit.nameSingular = nameSingular,
                                        UserUnit.namePlural = namePlural}
      Property.UserUnitProperty <$> parseChecked "user unit" payloadSize userUnit
    Property.VectorsType -> do
      payloadSize <- (Size . fromIntegral) <$> anyUword
      parseChecked "vectors" payloadSize $ do
        satisfying (fromIntegral <$> anyUword) ((==) 1)
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
                    satisfying (fromIntegral <$> anyUword) ((==) 1)
                    closed <- ((/=) 0 . fromIntegral) <$> anyUword
                    numFloats <- satisfying (fromIntegral <$> anyUword) (\n -> n >= 2 && n <= 6)
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
    Property.TextLayerFlagsType -> do
      satisfying (fromIntegral <$> anyUword) ((==) 4)
      (Property.TextLayerFlagsProperty . TextLayerFlags.TextLayerFlags) <$> anyUword
    Property.SamplePointsType -> undefined
    Property.LockContentType -> undefined
    Property.GroupItemType -> undefined
    Property.ItemPathType -> undefined
    Property.GroupItemFlagsType -> undefined
    
anyProperty :: Attoparsec.Parser Property.Property
anyProperty = do
  let allPropertyTypes = enumFromTo minBound maxBound :: [Property.Type]
  Attoparsec.choice $ map propertyOfType allPropertyTypes