module Codec.Image.XCF.Data.Version
       (Version (..),
        representation)
       where

import Data.ByteString.Char8

data Version = Version0 | Version1 | Version2 deriving (Bounded, Enum)

representation :: Version -> ByteString
representation Version0 = pack "file"
representation Version1 = pack "v001"
representation Version2 = pack "v002"