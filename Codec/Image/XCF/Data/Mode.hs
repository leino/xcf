{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.Mode
       (
         Mode (..)
       ) where

import Codec.Image.XCF.Represented
import Codec.Image.XCF.Data.Word

data Mode =
  Normal | 
  Dissolve |
  Behind |
  Multiply |
  Screen |
  Overlay |
  Difference |
  Addition |
  Subtract |
  DarkenOnly |
  LightenOnly |
  Hue |
  Saturation |
  Color |
  Value |
  Divide |
  Dodge |
  Burn |
  HardLight |
  SoftLight |
  GrainExtract |
  GrainMerge deriving (Bounded, Enum, Show, Eq)

instance Represented UWord Mode where
  representation = fromIntegral . fromEnum