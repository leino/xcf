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
  , allImageTypes
  , allLayerTypes
   )
       where

import Codec.Image.XCF.Data.Word
import qualified Codec.Image.XCF.Data.Path as Path
import qualified Codec.Image.XCF.Data.UserUnit as UserUnit
import qualified Codec.Image.XCF.Data.Parasite as Parasite
import qualified Codec.Image.XCF.Data.Vectors as Vectors
import qualified Codec.Image.XCF.Data.TextLayerFlags as TextLayerFlags
import qualified Codec.Image.XCF.Data.ColorMap as ColorMap
import qualified Codec.Image.XCF.Data.FloatingSelection as FloatingSelection
import qualified Codec.Image.XCF.Data.Mode as Mode
import qualified Codec.Image.XCF.Data.Offset as Offset
import qualified Codec.Image.XCF.Data.Color as Color
import qualified Codec.Image.XCF.Data.Opacity as Opacity

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

allImageTypes :: [Type]
allImageTypes = [
  ColorMapType,
  CompressionType,
  GuidesType,
  ResolutionType,
  UnitType,
  PathsType,
  UserUnitType,
  VectorsType
  ]

allLayerTypes :: [Type]
allLayerTypes = [
  OpacityType,
  VisibleType,
  LinkedType,
  TattooType,
  ParasitesType,
  EndType,
  ActiveLayerType,
  FloatingSelectionType,
  ModeType,
  LockAlphaType,
  ApplyMaskType,
  EditMaskType,
  ShowMaskType,
  OffsetsType,
  TextLayerFlagsType
  ]

data CompressionIndicator = None | RLE deriving (Bounded, Enum, Show)

data Property =
  EndProperty |
  ColorMapProperty ColorMap.ColorMap |
  ActiveLayerProperty |
  ActiveChannelProperty |
  SelectionProperty |
  FloatingSelectionProperty FloatingSelection.FloatingSelection |
  OpacityProperty Opacity.Opacity |
  ModeProperty Mode.Mode |
  VisibleProperty {isVisible :: Bool} |
  LinkedProperty {isLinked :: Bool} |
  LockAlphaProperty {alphaLocked :: Bool} |
  ApplyMaskProperty {shouldApplyLayerMask :: Bool} |
  EditMaskProperty {isBeingEdited :: Bool} |
  ShowMaskProperty {isShown :: Bool} |
  ShowMaskedProperty | 
  OffsetsProperty Offset.Offset |
  ColorProperty Color.Color |
  CompressionProperty CompressionIndicator |
  GuidesProperty [Guide] |
  ResolutionProperty {horizontalResolution :: Float, verticalResolution :: Float} |
  TattooProperty |
  ParasitesProperty [Parasite.Parasite] |
  UnitProperty Unit |
  PathsProperty Path.Paths |
  UserUnitProperty UserUnit.UserUnit |
  VectorsProperty Vectors.Vectors  |
  TextLayerFlagsProperty TextLayerFlags.TextLayerFlags |
  SamplePointsProperty |
  LockContentProperty |
  GroupItemProperty |
  ItemPathProperty |
  GroupItemFlagsProperty



-- TODO: move to own file under data
data GuideOrientation = Horizontal | Vertical deriving (Bounded, Enum, Show)

 -- TODO: move to own file under data
data Unit = Inches | Millimeters | Points | Picas deriving (Bounded, Enum, Show)

 -- TODO: move to own file under data
newtype GuideCoordinate = GuideCoordinate Word

 -- TODO: move to own file under data
data Guide = Guide GuideCoordinate GuideOrientation

 -- TODO: move to own file under data
instance Represented Word8 CompressionIndicator where
  representation = fromIntegral . fromEnum

 -- TODO: move to own file under data
instance Represented UWord Type where
  representation = UWord . fromIntegral . fromEnum

 -- TODO: move to own file under data
instance Represented Word8 GuideOrientation where
  representation = (+) 1 . fromIntegral . fromEnum

 -- TODO: move to own file under data
instance Represented UWord Unit where
  representation = (+) 1 . fromIntegral . fromEnum