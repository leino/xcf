module Codec.Image.XCF.Data.Level
       (Level (..), TilesPointer (..))
       where

import Codec.Image.XCF.Data.Word

newtype TilesPointer = TilesPointer UWord

data Level = Level {
  width :: Int,
  height :: Int,
  tilesPointer :: TilesPointer
  }