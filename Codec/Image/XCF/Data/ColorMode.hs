{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.ColorMode
       (OpaqueColorMode (..),
        ColorMode (..),
        bytesPerPixel,
        bytesPerOpaquePixel)
       where

import Codec.Image.XCF.Data.Word
import Codec.Image.XCF.Represented

data OpaqueColorMode = RGB | GrayScale | Indexed deriving (Bounded, Enum, Eq, Show)
data ColorMode = Alpha OpaqueColorMode | NoAlpha OpaqueColorMode

bytesPerOpaquePixel :: OpaqueColorMode -> Int
bytesPerOpaquePixel RGB = 3
bytesPerOpaquePixel GrayScale = 2
bytesPerOpaquePixel Indexed = 1

bytesPerPixel :: ColorMode -> Int
bytesPerPixel (NoAlpha cm) = bytesPerOpaquePixel cm
bytesPerPixel (Alpha cm) = 1 + bytesPerOpaquePixel cm

instance Represented UWord OpaqueColorMode where
  representation RGB = UWord 0
  representation GrayScale = UWord 1
  representation Indexed = UWord 2