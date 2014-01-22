module Codec.Image.XCF.Data.ColorMap
       (
         Color (..),
         ColorMap (..),
       ) where

import Data.Word

newtype ColorMap = ColorMap [Color]
data Color = Color {
  red :: Word8,
  green :: Word8,
  blue :: Word8
  }