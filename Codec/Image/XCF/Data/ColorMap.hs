module Codec.Image.XCF.Data.ColorMap
       (
         Color (..),
         ColorMap (..),
       ) where

import Data.Word

newtype ColorMap = ColorMap [Color] deriving Show

data Color = Color {
  red :: Word8,
  green :: Word8,
  blue :: Word8
  } deriving (Show, Eq)