{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.CompressionIndicator
       (CompressionIndicator (..))
       where

import Codec.Image.XCF.Represented

import Data.Word
data CompressionIndicator = None | RLE deriving (Bounded, Enum, Show)

 -- TODO: move to own file under data
instance Represented Word8 CompressionIndicator where
  representation = fromIntegral . fromEnum
