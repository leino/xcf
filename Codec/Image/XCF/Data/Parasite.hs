module Codec.Image.XCF.Data.Parasite
       (Parasite (..))
       where

import Codec.Image.XCF.Data.Word
import Data.ByteString
import Data.Text

data Parasite = Parasite {name :: Text, flags :: UWord, payload :: ByteString} deriving Show