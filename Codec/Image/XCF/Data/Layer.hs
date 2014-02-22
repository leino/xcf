{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.Layer
       (
         Layer (..),
         Type (..),
         HierarchyPointer (..),
         LayerMaskPointer (..),
         bytesPerPixel,
         offsetInCanvas,
         )
       where

import Data.Text hiding (find)
import Data.List (find)
import Codec.Image.XCF.Data.Word
import qualified Codec.Image.XCF.Data.Offset as Offset
import Codec.Image.XCF.Represented
import qualified Codec.Image.XCF.Data.Property as Property

newtype HierarchyPointer = HierarchyPointer UWord deriving (Show, Eq)
newtype LayerMaskPointer = LayerMaskPointer UWord deriving (Show, Eq)

data Type = RGB |
            RGBAlpha |
            GrayScale |
            GrayScaleAlpha |
            Indexed |
            IndexedAlpha deriving (Enum, Bounded, Show, Eq)

bytesPerPixel :: Type -> Int
bytesPerPixel RGB = 3
bytesPerPixel RGBAlpha = 4
bytesPerPixel GrayScale = 1
bytesPerPixel GrayScaleAlpha = 2
bytesPerPixel Indexed = 1
bytesPerPixel IndexedAlpha = 2

data Layer = Layer {
  width :: Int,
  height :: Int,
  layerType :: Type,
  name :: Text,
  properties :: [Property.Property],
  hierarchyPointer :: HierarchyPointer,
  layerMaskPointer :: Maybe LayerMaskPointer
  } deriving Show


isOffsetsProperty (Property.OffsetsProperty _) = True
isOffsetsProperty _ = False

-- Offset of this layer's top left corner, relative to top left corner of the canvas
-- (x-coordinate going to the right, and y-coordinate going down)
offsetInCanvas :: Layer -> Offset.Offset
offsetInCanvas layer =
  case find isOffsetsProperty (properties layer) of
    Nothing -> Offset.Offset 0 0
    Just (Property.OffsetsProperty offset) -> offset

instance Represented UWord Type where
  representation = fromIntegral . fromEnum