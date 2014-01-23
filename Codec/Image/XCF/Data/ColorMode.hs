{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.ColorMode
       (ColorMode (..))
       where

import Codec.Image.XCF.Data.Word
import Codec.Image.XCF.Represented
data ColorMode = RGB | GrayScale | Indexed deriving (Bounded, Enum, Eq, Show)

instance Represented UWord ColorMode where
  representation RGB = UWord 0
  representation GrayScale = UWord 1
  representation Indexed = UWord 2