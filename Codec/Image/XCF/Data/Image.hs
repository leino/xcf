module Codec.Image.XCF.Data.Image
       (
         Image (..)
       )
       where

import qualified Codec.Image.XCF.Data as Data
import qualified Codec.Image.XCF.Data.Version as Version
import qualified Codec.Image.XCF.Data.ColorMode as ColorMode
import qualified Codec.Image.XCF.Data.Property as Property

data Image = Image {
  version :: Version.Version,
  colorMode :: ColorMode.OpaqueColorMode,
  imageProperties :: [Property.Property],
  channelPointers :: [Data.ChannelPointer],
  layerPointers :: [Data.LayerPointer]
  } deriving Show