module Codec.Image.XCF.Data.Level
       (Level (..), TilePointer (..))
       where

import Codec.Image.XCF.Data.Word

newtype TilePointer = TilePointer UWord deriving (Show, Eq)

data Level = Level {
  width :: Int,
  height :: Int,
  tilePointers :: [TilePointer]
  } deriving Show