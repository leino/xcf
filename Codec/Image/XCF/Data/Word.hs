{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.Image.XCF.Data.Word
       (UWord (..), Word (..), uWordSize)
       where

import Data.Word hiding (Word)
import Data.Int

uWordSize :: Int64
uWordSize = 4

newtype UWord = UWord Word32 deriving (Real, Ord, Enum, Eq, Num, Integral, Show)
newtype Word = Word Word32
