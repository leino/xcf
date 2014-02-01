module Codec.Image.XCF.Data.Hierarchy
       (Hierarchy (..), LevelPointer (..))
       where

import Codec.Image.XCF.Data.Word

newtype LevelPointer = LevelPointer UWord deriving (Eq, Show)

data Hierarchy = Hierarchy {
  width :: Int,
  height :: Int,
  bytesPerPixel :: Int,
  levelPointer :: LevelPointer
  } deriving Show