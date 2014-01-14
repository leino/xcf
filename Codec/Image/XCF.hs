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
import Data.Attoparsec as Attoparsec
import Data.Attoparsec.Binary
values :: (Bounded a, Enum a) => [a]
values = enumFromTo minBound maxBound

data Color = Color RGB |
             GrayscaleColor Intensity |
             IndexedColor ColorMapIndex

data RGB = RGB Intensity Intensity Intensity

newtype Intensity = Intensity Word8

newtype ColorMapIndex = ColorMapIndex Word8

newtype ColorMap = ColorMap (ColorMapIndex -> RGB)
newtype Alpha = Alpha Intensity
data Pixel = Pixel Alpha Color
anyUword :: Parser UWord
anyUword = UWord <$> anyWord32be

uWord :: UWord -> Parser UWord
uWord (UWord w)  = UWord <$> word32be w

anyWord :: Parser Word
anyWord = Word <$> anyWord32be

word :: Word -> Parser Word
word (Word w) = Word <$> word32be w
string :: Parser Text.Text
string =
  nonEmptyString <|> emptyString
  where
    nonEmptyString :: Parser Text.Text
    nonEmptyString = do
      n <- fromIntegral <$> anyUword
      t <- utf8 n
      word8 0
      return t
      where
        utf8 :: Int -> Parser Text.Text
        utf8 n =
          (decodeUtf8' <$> Attoparsec.take n) >>= either (fail . show) return
          <?>
          "string (UTF8)"
        
    emptyString :: Parser Text.Text
    emptyString = word8 0 >> (return $ Text.empty)
    
newtype ParasiteIdentifier = ParasiteIdentifier Text.Text
data Parasite = Parasite ParasiteIdentifier
version :: Version.Version -> Parser Version.Version
version v = do
  Attoparsec.string $ Version.representation v
  return v
anyVersion :: Parser Version.Version
anyVersion = do
  let allVersions = values :: [Version.Version]
  (choice $ map version allVersions) <|>
    ((CharString.unpack <$> Attoparsec.take 4) >>= fail . (++) "unknown version: ") -- tack on a fail parser
colorMode :: ColorMode.ColorMode -> Parser ColorMode.ColorMode
colorMode m = do
  uWord $ ColorMode.representation m
  return m
anyColorMode :: Parser ColorMode.ColorMode
anyColorMode = do
  let allModes = enumFromTo minBound maxBound :: [ColorMode.ColorMode]
  choice $ map colorMode allModes
newtype LayerPointer = LayerPointer UWord
newtype ChannelPointer = ChannelPointer UWord
anyLayerPointer :: Parser LayerPointer
anyLayerPointer = LayerPointer <$> anyUword

anyChannelPointer :: Parser ChannelPointer
anyChannelPointer = ChannelPointer <$> anyUword
anyImageProperty = undefined
image :: Parser Image.Image
image = do
  Attoparsec.string $ CharString.pack "gimp xcf "
  v <- anyVersion
  word8 0
  width <- anyUword
  height <- anyUword
  mode <- anyColorMode
  imgprops <- many' anyImageProperty
  layerptrs <- many' anyLayerPointer
  word8 0
  channelptrs <- many' anyChannelPointer
  word8 0
  return undefined
propertyType :: Property.Type -> Parser Property.Type
propertyType t = do
  uWord $ Property.representation t
  return t
compressionIndicator :: Property.CompressionIndicator -> Parser Property.CompressionIndicator
compressionIndicator i = word8 (Property.representation i) >> return i

guideOrientation :: Property.GuideOrientation -> Parser Property.GuideOrientation
guideOrientation o = word8 (Property.representation o) >> return o

unit :: Property.Unit -> Parser Property.Unit
unit u = uWord (Property.representation u) >> return u
anyCompressionIndicator :: Parser Property.CompressionIndicator
anyCompressionIndicator = choice $ map compressionIndicator values
anyGuideOrientation :: Parser Property.GuideOrientation
anyGuideOrientation = choice $ map guideOrientation values

anyUnit :: Parser Property.Unit
anyUnit = choice $ map unit values
anyGuide :: Parser Property.Guide
anyGuide = do
  c <- (Property.GuideCoordinate <$> anyWord)
  o <- anyGuideOrientation
  return $ Property.Guide c o
satisfying :: Parser a -> (a -> Bool) -> Parser a
satisfying p cond = do
  x <- p
  if cond x then return x else fail "condition could not be satisfied"
divisible :: Integral a => a -> a -> Bool
divisible n x = x `mod` n == 0
anyFloat :: Parser Float
anyFloat = undefined
propertyOfType :: Property.Type -> Parser Property.Property
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
      Property.GuidesProperty <$> count n anyGuide
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
    Property.ParasitesType -> undefined
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
anyProperty :: Parser Property.Property
anyProperty = do
  let allPropertyTypes = enumFromTo minBound maxBound :: [Property.Type]
  choice $ map propertyOfType allPropertyTypes