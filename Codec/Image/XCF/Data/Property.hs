{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.Property (
    Property (..)
  , Type (..)
  , CompressionIndicator (..)
  , Represented (..)
  , Guide (..)
  , GuideCoordinate (..)
  , GuideOrientation (..)
  , Unit (..)
  , Parasite (..)
   )
       where

import Codec.Image.XCF.Data.Word
import qualified Codec.Image.XCF.Data.Path as Path
import Codec.Image.XCF.Represented
import Data.ByteString
import Data.Text
import Data.Word hiding (Word)

-- Basically a transcription from gimp-2.8.6/app/xcf/xcf-private.h
data Type =
  EndType |
  ColorMapType |
  ActiveLayerType |
  ActiveChannelType |
  SelectionType |
  FloatingSelectionType |
  OpacityType |
  ModeType |
  VisibleType |
  LinkedType |
  LockAlphaType |
  ApplyMaskType |
  EditMaskType |
  ShowMaskType |
  ShowMaskedType | 
  OffsetsType |
  ColorType |
  CompressionType |
  GuidesType |
  ResolutionType |
  TattooType |
  ParasitesType |
  UnitType |
  PathsType |
  UserUnitType |
  VectorsType |
  TextLayerFlagsType |
  SamplePointsType |
  LockContentType |
  GroupItemType |
  ItemPathType |
  GroupItemFlagsType
  deriving (Bounded, Enum, Show)

data CompressionIndicator = None | RLE deriving (Bounded, Enum, Show)

data Property =
  EndProperty |
  ColorMapProperty |
  ActiveLayerProperty |
  ActiveChannelProperty |
  SelectionProperty |
  FloatingSelectionProperty |
  OpacityProperty |
  ModeProperty |
  VisibleProperty |
  LinkedProperty |
  LockAlphaProperty |
  ApplyMaskProperty |
  EditMaskProperty |
  ShowMaskProperty |
  ShowMaskedProperty | 
  OffsetsProperty |
  ColorProperty |
  CompressionProperty CompressionIndicator |
  GuidesProperty [Guide] |
  ResolutionProperty {horizontalResolution :: Float, verticalResolution :: Float} |
  TattooProperty |
  ParasitesProperty [Parasite] |
  UnitProperty Unit |
  PathsProperty Path.Paths |
  UserUnitProperty |
  VectorsProperty |
  TextLayerFlagsProperty |
  SamplePointsProperty |
  LockContentProperty |
  GroupItemProperty |
  ItemPathProperty |
  GroupItemFlagsProperty

data GuideOrientation = Horizontal | Vertical deriving (Bounded, Enum, Show)
data Unit = Inches | Millimeters | Points | Picas deriving (Bounded, Enum, Show)

data Parasite = Parasite {name :: Text, flags :: Word8, payload :: ByteString}

newtype GuideCoordinate = GuideCoordinate Word
data Guide = Guide GuideCoordinate GuideOrientation

instance Represented Word8 CompressionIndicator where
  representation = fromIntegral . fromEnum
  
instance Represented UWord Type where
  representation = UWord . fromIntegral . fromEnum
  
instance Represented Word8 GuideOrientation where
  representation = (+) 1 . fromIntegral . fromEnum

instance Represented UWord Unit where
  representation = (+) 1 . fromIntegral . fromEnum