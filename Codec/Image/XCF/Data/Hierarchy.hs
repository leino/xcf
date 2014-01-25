module Codec.Image.XCF.Data.Hierarchy
       (Hierarchy (..), LevelPointer (..))
       where

import Codec.Image.XCF.Data.Word

newtype LevelPointer = LevelPointer UWord

data Hierarchy = Hierarchy {
  width :: Int,
  height :: Int,
  bytesPerPixel :: Int,
  levelPointer :: LevelPointer
  }