{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.Layer
       (Layer (..), Type (..), HierarchyPointer (..), LayerMaskPointer (..))
       where

import Data.Text
import Codec.Image.XCF.Data.Word
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

data Layer = Layer {
  width :: Int,
  height :: Int,
  layerType :: Type,
  name :: Text,
  properties :: [Property.Property],
  hierarchyPointer :: HierarchyPointer,
  layerMaskPointer :: Maybe LayerMaskPointer
  } deriving Show

instance Represented UWord Type where
  representation = fromIntegral . fromEnum