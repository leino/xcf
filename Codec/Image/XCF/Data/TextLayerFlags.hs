module Codec.Image.XCF.Data.TextLayerFlags
       (
         TextLayerFlags (..),
       )
       where

import Codec.Image.XCF.Data.Word

newtype TextLayerFlags = TextLayerFlags UWord deriving (Show, Eq)