module Codec.Image.XCF.Data
       (
         module Codec.Image.XCF.Data.Property,
         LayerPointer (..),
         ChannelPointer (..),
       )
       where

import Data.Word
import Codec.Image.XCF.Data.Property
import Codec.Image.XCF.Data.Word

newtype LayerPointer = LayerPointer UWord deriving (Show, Eq)
newtype ChannelPointer = ChannelPointer UWord deriving (Show, Eq)