module Codec.Image.XCF where
import Control.Applicative ((<$>), (<|>))
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as CharString
import Data.Text.Encoding
import Data.Word hiding (Word)
import qualified Codec.Image.XCF.Data.ColorMode as ColorMode
import qualified Codec.Image.XCF.Data.Image as Image
import qualified Codec.Image.XCF.Data.Property as Property
import qualified Codec.Image.XCF.Data.Version as Version
import Codec.Image.XCF.Data.Word
import qualified Data.Attoparsec as Attoparsec
import Data.Attoparsec.Binary

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

values :: (Bounded a, Enum a) => [a]
values = enumFromTo minBound maxBound

anyUword :: Attoparsec.Parser UWord
anyUword = UWord <$> anyWord32be

uWord :: UWord -> Attoparsec.Parser UWord
uWord (UWord w)  = UWord <$> word32be w

anyWord :: Attoparsec.Parser Word
anyWord = Word <$> anyWord32be

word :: Word -> Attoparsec.Parser Word
word (Word w) = Word <$> word32be w

string :: Attoparsec.Parser Text.Text
string =
  nonEmptyString <|> emptyString
  where
    nonEmptyString :: Attoparsec.Parser Text.Text
    nonEmptyString = do
      n <- fromIntegral <$> anyUword
      t <- utf8 n
      Attoparsec.word8 0
      return t
      where
        utf8 :: Int -> Attoparsec.Parser Text.Text
        utf8 n =
          (decodeUtf8' <$> Attoparsec.take n) >>= either (fail . show) return
          Attoparsec.<?>
          "string (UTF8)"
        
    emptyString :: Attoparsec.Parser Text.Text
    emptyString = Attoparsec.word8 0 >> (return $ Text.empty)
    
version :: Version.Version -> Attoparsec.Parser Version.Version
version v = do
  Attoparsec.string $ Version.representation v
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
  
satisfying :: Attoparsec.Parser a -> (a -> Bool) -> Attoparsec.Parser a
satisfying p cond = do
  x <- p
  if cond x then return x else fail "condition could not be satisfied"
  
divisible :: Integral a => a -> a -> Bool
divisible n x = x `mod` n == 0

anyFloat :: Attoparsec.Parser Float
anyFloat = undefined

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
        Property.horizontalResolution = x
        ,Property.verticalResolution = y
        }
    Property.TattooType -> do
      satisfying (fromIntegral <$> anyUword) ((==) 4)
      Property.UnitProperty <$> anyUnit
    Property.ParasitesType -> do
      l <- fromIntegral <$> anyUword
      -- bit of an issue here: need to parse exactly l bytes
      -- on the following format:
      let parasite = do
            name <- string
            flags <- anyUword
            n <- fromIntegral <$> anyUword
            bs <- Attoparsec.take n
            return $ Property.Parasite name
      return undefined
    Property.UnitType -> undefined
    Property.PathsType -> undefined
    Property.UserUnitType -> undefined
    Property.VectorsType -> undefined
    Property.TextLayerFlagsType -> undefined
    Property.SamplePointsType -> undefined
    Property.LockContentType -> undefined
    Property.GroupItemType -> undefined
    Property.ItemPathType -> undefined
    Property.GroupItemFlagsType -> undefined
    
anyProperty :: Attoparsec.Parser Property.Property
anyProperty = do
  let allPropertyTypes = enumFromTo minBound maxBound :: [Property.Type]
  Attoparsec.choice $ map propertyOfType allPropertyTypes