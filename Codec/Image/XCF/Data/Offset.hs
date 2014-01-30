module Codec.Image.XCF.Data.Offset
       (
         Offset (..)
       ) where

data Offset = Offset {
  xOffset :: Int,
  yOffset :: Int
  } deriving Show