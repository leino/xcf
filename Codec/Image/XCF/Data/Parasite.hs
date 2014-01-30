module Codec.Image.XCF.Data.Parasite
       (Parasite (..))
       where

import Data.Word
import Data.ByteString
import Data.Text

data Parasite = Parasite {name :: Text, flags :: Word8, payload :: ByteString} deriving Show