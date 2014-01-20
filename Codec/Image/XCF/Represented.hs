{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Image.XCF.Represented where

class Represented a b where
  representation :: b -> a
