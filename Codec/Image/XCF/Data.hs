module Codec.Image.XCF.Data
       (
         module Codec.Image.XCF.Data.Property,
         LayerPointer (..),
         ChannelPointer (..),
       )
       where

import Data.Word
import Data.Text
import Codec.Image.XCF.Data.Property
import Codec.Image.XCF.Data.Word

data Color = Color RGB |
             GrayscaleColor Intensity |
             IndexedColor ColorMapIndex
data RGB = RGB Intensity Intensity Intensity
newtype Intensity = Intensity Word8
newtype ColorMapIndex = ColorMapIndex Word8
newtype ColorMap = ColorMap (ColorMapIndex -> RGB)
newtype Alpha = Alpha Intensity
data Pixel = Pixel Alpha Color
newtype ParasiteIdentifier = ParasiteIdentifier Text
data Parasite = Parasite ParasiteIdentifier
newtype LayerPointer = LayerPointer UWord
newtype ChannelPointer = ChannelPointer UWord