{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Data.Version
       (Version (..))
       where

import Data.ByteString.Char8
import Codec.Image.XCF.Represented

data Version = Version0 | Version1 | Version2 deriving (Bounded, Enum, Eq, Show)

instance Represented ByteString Version where
  representation Version0 = pack "file"
  representation Version1 = pack "v001"
  representation Version2 = pack "v002"