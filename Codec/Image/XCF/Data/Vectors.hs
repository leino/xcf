{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.Vectors
       (
         PointType (..),
         ControlPoint (..),
         Stroke (..),
         Path (..),
         Vectors (..)
       )
       where

import Data.Text
import Codec.Image.XCF.Represented
import Codec.Image.XCF.Data.Word
import Codec.Image.XCF.Data.Tattoo
import Codec.Image.XCF.Data.Parasite

data PointType = Anchor | Bezier deriving (Enum, Eq, Bounded, Show)

data ControlPoint =
  ControlPoint {
    pointType :: PointType,
    x :: Float,
    y :: Float,
    pressure :: Float,
    xTilt :: Float,
    yTilt :: Float,
    wheel :: Float
    }

data Stroke = Stroke {
  closed :: Bool,
  controlPoints :: [ControlPoint]
  }

data Path = Path {
  name :: Text,
  tattoo :: Maybe Tattoo,
  visible :: Bool,
  linked :: Bool,
  parasites :: [Parasite],
  strokes :: [Stroke]}

data Vectors = Vectors {
  activePathIdx :: Int,
  paths :: [Path]
  }

instance Represented UWord PointType where
  representation = fromIntegral . fromEnum