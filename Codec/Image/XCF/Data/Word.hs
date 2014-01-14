{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.Image.XCF.Data.Word
       (UWord (..), Word (..))
       where

import Data.Word hiding (Word)

newtype UWord = UWord Word32 deriving (Real, Ord, Enum, Eq, Num, Integral, Show)
newtype Word = Word Word32
