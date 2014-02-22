import Control.Applicative ((<$>), pure)
import qualified Data.Attoparsec as Attoparsec
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as CharString
import Data.Maybe (fromJust)
import Codec.BMP
import qualified Codec.Image.XCF as XCF
import qualified Codec.Image.XCF.Data.Image as Image
import qualified Codec.Image.XCF.Data.Layer as Layer
import qualified Codec.Image.XCF.Data.Hierarchy as Hierarchy
import qualified Codec.Image.XCF.Data.Tile as Tile
import qualified Codec.Image.XCF.Data.Offset as Offset
import qualified Codec.Image.XCF.Data.ColorMode as ColorMode
import qualified Codec.Image.XCF.Data.Property as Property
import System.Environment (getArgs)

data Bitmap = Bitmap {width :: Int, height :: Int, bytes :: ByteString.ByteString}
data BMPFile = BMPFile {name :: String, bmp :: BMP} deriving Show
data Rectangle = Rectangle Int Int Int Int deriving Show

intersect :: Rectangle -> Rectangle -> Rectangle
intersect (Rectangle x y w h) (Rectangle x' y' w' h') =
  let left = max x x'
      right = min (x + w) (x' + w')
      top = max y y'
      bottom = min (y + h) (y' + h')
  in
  Rectangle left top (right - left) (bottom - top)

copyDropy :: Int -> Int -> Int -> ByteString.ByteString -> ByteString.ByteString
copyDropy copyStride dropyStride 0 bs = ByteString.empty
copyDropy copyStride dropyStride numTimes bs =
  let rbs = ByteString.take copyStride bs in
  ByteString.append rbs $
  copyDropy copyStride dropyStride (numTimes-1) (ByteString.drop (copyStride + dropyStride) bs)

-- crop a bitmap to a rectangle, which is assumed to be contained entirely within the bitmap
cropBitmap :: Rectangle -> Bitmap -> Bitmap
cropBitmap (Rectangle x y rw rh) (Bitmap bw bh bs) =
  let bpp = 4 -- assuming 4 bytes per pixel
      offsetAt x y = (y*bw + x)*bpp
      topLeftOffset = offsetAt x y
      bottomRightOffset = offsetAt (x+rw) (y+rh)
      rowStride = bpp*rw
      dropStride = (offsetAt x (y+1)) - (offsetAt (x+rw) y) -- amount of bytes to drop after reading each row
      bs' = copyDropy rowStride dropStride rh (ByteString.drop topLeftOffset bs)
  in
   Bitmap rw rh bs'

main =
  head <$> getArgs >>= ByteString.readFile >>= \bs -> do
    (pure $ XCF.parse bs) >>=  \(Attoparsec.Done _ img) -> do
      let compressionIndicator = fromJust $ Image.compressionIndicator img
          canvasWidth = Image.width img
          canvasHeight = Image.height img
          layers = [
            let (Attoparsec.Done _ layer) = XCF.parseLayerAt lp bs in layer
            | lp <- Image.layerPointers img
            ]
          hierarchies = [
            let (Attoparsec.Done _ hierarchy) = XCF.parseHierarchyAt hp bs in hierarchy
            | hp <- map Layer.hierarchyPointer layers]
          levels = [
            let (Attoparsec.Done _ level) = XCF.parseLevelAt lp bs in level
            | lp <- map Hierarchy.levelPointer hierarchies
            ]
          tiless = [
            let (Attoparsec.Done _ tiles) = XCF.parseTiles compressionIndicator layerType level bs in tiles
            | (layerType, level) <- zip (map Layer.layerType layers) levels
            ]
          bitmaps = [
            let width = Hierarchy.width $ hierarchy
                height = Hierarchy.height $ hierarchy
                bs = Tile.decodeTiles width height tiles in
            Bitmap width height bs
            | (hierarchy, tiles) <- zip hierarchies tiless
            ]
          offsets = map Layer.offsetInCanvas layers
          croppedBitmaps = [
            let canvasRectangle = Rectangle (-dx) (-dy) canvasWidth canvasHeight
                bitmapRectangle = Rectangle 0 0 w h
                cropRectangle = intersect canvasRectangle bitmapRectangle
            in
             cropBitmap cropRectangle bitmap
            | (Offset.Offset dx dy, bitmap@(Bitmap w h bs)) <- zip offsets bitmaps
            ]
          bmps = [
            BMPFile {
               name = concat ["layer_", show i, ".bmp"],
               bmp = packRGBA32ToBMP width height bs 
               }
            | (Bitmap width height bs, i) <- zip croppedBitmaps [0 ..]
            ]
      mapM_ print offsets
      mapM_ (\(BMPFile filename bmp) -> writeBMP filename bmp) bmps