module Codec.Image.XCF.Data.ColorMode
       (ColorMode (..),
        representation)
       where

import Codec.Image.XCF.Data.Word
data ColorMode = RGB | GrayScale | Indexed deriving (Bounded, Enum, Eq, Show)

representation :: ColorMode -> UWord
representation RGB = UWord 0
representation GrayScale = UWord 1
representation Indexed = UWord 2