{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.Property (
    Property (..)
  , Type (..)
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
import qualified Codec.Image.XCF.Data.Channel as Channel
import qualified Codec.Image.XCF.Data.Opacity as Opacity
import qualified Codec.Image.XCF.Data.Tattoo as Tattoo
import qualified Codec.Image.XCF.Data.CompressionIndicator as CompressionIndicator

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
  TattooType,
  ParasitesType,
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
  ActiveLayerType,
  FloatingSelectionType,
  ModeType,
  LockAlphaType,
  ApplyMaskType,
  EditMaskType,
  ShowMaskType,
  OffsetsType,
  TextLayerFlagsType,
  LockContentType
  ]



data Property =
  EndProperty | -- 0
  ColorMapProperty ColorMap.ColorMap |
  ActiveLayerProperty |
  ActiveChannelProperty |
  SelectionProperty |
  FloatingSelectionProperty FloatingSelection.FloatingSelection | -- 5
  OpacityProperty Opacity.Opacity |
  ModeProperty Mode.Mode |
  VisibleProperty {isVisible :: Bool} |
  LinkedProperty {isLinked :: Bool} |
  LockAlphaProperty {alphaLocked :: Bool} | -- 10
  ApplyMaskProperty {shouldApplyLayerMask :: Bool} |
  EditMaskProperty {isBeingEdited :: Bool} |
  ShowMaskProperty {isShown :: Bool} |
  ShowMaskedProperty | 
  OffsetsProperty Offset.Offset | -- 15
  ColorProperty Channel.Color |
  CompressionProperty CompressionIndicator.CompressionIndicator |
  GuidesProperty [Guide] |
  ResolutionProperty {horizontalResolution :: Float, verticalResolution :: Float} |
  TattooProperty Tattoo.Tattoo | --20
  ParasitesProperty [Parasite.Parasite] |
  UnitProperty Unit |
  PathsProperty Path.Paths |
  UserUnitProperty UserUnit.UserUnit |
  VectorsProperty Vectors.Vectors  | --25
  TextLayerFlagsProperty TextLayerFlags.TextLayerFlags |
  SamplePointsProperty |
  LockContentProperty {isContentLocked :: Bool} |
  GroupItemProperty |
  ItemPathProperty | -- 30
  GroupItemFlagsProperty deriving Show



-- TODO: move to own file under data
data GuideOrientation = Horizontal | Vertical deriving (Bounded, Enum, Show)

 -- TODO: move to own file under data
data Unit = Inches | Millimeters | Points | Picas deriving (Bounded, Enum, Show)

 -- TODO: move to own file under data
newtype GuideCoordinate = GuideCoordinate Word deriving Show

 -- TODO: move to own file under data
data Guide = Guide GuideCoordinate GuideOrientation deriving Show

 -- TODO: move to own file under data
instance Represented UWord Type where
  representation = UWord . fromIntegral . fromEnum

 -- TODO: move to own file under data
instance Represented Word8 GuideOrientation where
  representation = (+) 1 . fromIntegral . fromEnum

 -- TODO: move to own file under data
instance Represented UWord Unit where
  representation = (+) 1 . fromIntegral . fromEnum