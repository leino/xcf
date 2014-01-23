module Codec.Image.XCF.Data.Image
       (
         Image (..)
       )
       where

import qualified Codec.Image.XCF.Data.ColorMode as ColorMode

data Image = Image {
  colorMode :: ColorMode.ColorMode
  }