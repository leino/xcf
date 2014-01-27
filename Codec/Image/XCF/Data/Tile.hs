module Codec.Image.XCF.Data.Tile
       (
         Run (..),
         Tiles (..),
       )
       where

import Data.Word
import Data.ByteString
import Prelude hiding (length)

data Run = Run Int Word8 | Block ByteString

data Tiles =
  RawTiles ByteString | 
  RGBTiles [([Run], [Run], [Run])] |
  RGBAlphaTiles [([Run], [Run], [Run], [Run])] |
  GrayscaleTiles [[Run]] |
  GrayscaleAlphaTiles [([Run], [Run])] |
  IndexedTiles [[Run]] |
  IndexedAlphaTiles [([Run], [Run])]