module Codec.Image.XCF.Data.Image
       (
         Image (..), compressionIndicator
       )
       where

import qualified Codec.Image.XCF.Data as Data
import qualified Codec.Image.XCF.Data.Version as Version
import qualified Codec.Image.XCF.Data.ColorMode as ColorMode
import qualified Codec.Image.XCF.Data.Property as Property
import qualified Codec.Image.XCF.Data.CompressionIndicator as CompressionIndicator
import Data.List (find)

data Image = Image {
  version :: Version.Version,
  colorMode :: ColorMode.OpaqueColorMode,
  width :: Int,
  height :: Int,
  imageProperties :: [Property.Property],
  channelPointers :: [Data.ChannelPointer],
  layerPointers :: [Data.LayerPointer]
  } deriving Show

isCompressionProperty :: Property.Property -> Bool
isCompressionProperty (Property.CompressionProperty _) = True
isCompressionProperty _ = False

compressionIndicator :: Image -> Maybe CompressionIndicator.CompressionIndicator
compressionIndicator img = do
  (Property.CompressionProperty compressionIndicator) <- do
    find isCompressionProperty $ imageProperties img
  return compressionIndicator
