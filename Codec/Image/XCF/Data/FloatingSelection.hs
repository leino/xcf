module Codec.Image.XCF.Data.FloatingSelection
       (
         FloatingSelection (..),
       ) where

import Codec.Image.XCF.Data.Word

-- A floating selection is just a pointer to the layer or channel that the selection is atteched to.
newtype FloatingSelection = FloatingSelection {
  pointer :: UWord
  } deriving Show