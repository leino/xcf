{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.Path
       (Version (..),
        PointType (..),
        Point (..),
        Path (..),
        Paths (..))
       where

import Data.Word
import Codec.Image.XCF.Represented
import Codec.Image.XCF.Data.Word

data Version = Integral | Float | Tattoo deriving (Bounded, Enum, Show, Eq)
data PointType = Anchor | Bezier deriving (Bounded, Enum, Show, Eq)

data Point n = Point PointType n n 
data Path = IntegralPath [Point Int] |
            FloatPath [Point Float] |
            TattooPath [Point Float]
data Paths = Paths {activeIdx :: Int, paths :: [Path]}

instance Represented Word8 Version where
  representation = (+) 1 . fromIntegral . fromEnum

instance Represented UWord PointType where
  representation = fromIntegral . fromEnum