module Codec.Image.XCF.Data.Tile
       (Tiles (..), Run (..))
       where

import Data.Word
import Data.ByteString
import Prelude hiding (length)

data Run = Run Int Word8 | Block ByteString
data Tiles = RawTiles ByteString | RLETiles [Run]