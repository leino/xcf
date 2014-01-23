module Codec.Image.XCF.Data.Image
       (
         Image (..)
       )
       where

import qualified Codec.Image.XCF.Data.Version as Version
import qualified Codec.Image.XCF.Data.ColorMode as ColorMode

data Image = Image {
  version :: Version.Version,
  colorMode :: ColorMode.ColorMode
  }