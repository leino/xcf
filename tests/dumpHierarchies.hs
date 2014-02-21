import Control.Applicative ((<$>), pure)
import System.Environment (getArgs)
import Codec.BMP
import Codec.Image.XCF
import qualified Codec.Image.XCF.Data.Image as Image
import Codec.Image.XCF.Data.Layer (hierarchyPointer)
import qualified Codec.Image.XCF.Data.Hierarchy as Hierarchy
import Codec.Image.XCF.Data.Level
import Codec.Image.XCF.Data.Tile
import qualified Codec.Image.XCF.Data.ColorMode as ColorMode
import qualified Codec.Image.XCF.Data.Property as Property
import qualified Data.Attoparsec as Attoparsec
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as CharString
import Data.Maybe (fromJust)

main =
  head <$> getArgs >>= ByteString.readFile >>= \bs -> do
    (pure $ parse bs) >>=  \(Attoparsec.Done _ img) -> do
      let compressionIndicator = fromJust $ Image.compressionIndicator img
          colorModeOpaque = Image.colorMode img
          colorMode = ColorMode.NoAlpha colorModeOpaque          
          layers = [
            let (Attoparsec.Done _ layer) = parseLayerAt lp bs in layer
            | lp <- Image.layerPointers img
            ]
          hierarchies = [
            let (Attoparsec.Done _ hierarchy) = parseHierarchyAt hp bs in hierarchy
            | hp <- map hierarchyPointer layers]
          levels = [
            let (Attoparsec.Done _ level) = parseLevelAt lp bs in level
            | lp <- map Hierarchy.levelPointer hierarchies
            ]
          tiless = [
            let (Attoparsec.Done _ tiles) = parseTiles compressionIndicator colorMode level bs in tiles
            | level <- levels
            ]
      sequence_ [
        do
          let width = Hierarchy.width $ hierarchy
              height = Hierarchy.height $ hierarchy
              bs = decodeTiles width height tiles
              bmp = packRGBA32ToBMP width height bs 
              filename = concat ["layer_", show i, ".bmp"]
          putStrLn "width: " >> print width
          putStrLn "height: " >> print height
          writeBMP filename bmp
        | (hierarchy, tiles, i) <- zip3 hierarchies tiless [0 ..]
        ]