module Codec.Image.XCF.Data.Color
       (
         Color (..),
       ) where

import Data.Word

data Color = Color {
  red :: Word8,
  green :: Word8,
  blue :: Word8
  } deriving (Show, Eq)