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

newtype LayerPointer = LayerPointer UWord
newtype ChannelPointer = ChannelPointer UWord