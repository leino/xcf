import Control.Applicative ((<$>), pure)
import System.Environment (getArgs)
import Codec.Image.XCF
import qualified Codec.Image.XCF.Data.Image as Image
import Codec.Image.XCF.Data.Layer (hierarchyPointer)
import Codec.Image.XCF.Data.Hierarchy
import Codec.Image.XCF.Data.Level
import qualified Codec.Image.XCF.Data.Property as Property
import qualified Data.Attoparsec as Attoparsec
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as CharString
import Data.Maybe (fromJust)

main =
  head <$> getArgs >>= ByteString.readFile >>= \bs -> do
    (pure $ parse bs) >>=  \(Attoparsec.Done _ img) -> do
      let compressionIndicator = fromJust $ Image.compressionIndicator img
          layers = [
            let (Attoparsec.Done _ layer) = parseLayerAt lp bs in layer
            | lp <- Image.layerPointers img
            ]
          hierarchies = [
            let (Attoparsec.Done _ hierarchy) = parseHierarchyAt hp bs in hierarchy
            | hp <- map hierarchyPointer layers]
          levels = [
            let (Attoparsec.Done _ level) = parseLevelAt lp bs in level
            | lp <- map levelPointer hierarchies
            ]
          tiless = [
            let (Attoparsec.Done _ tiles) = parseTiles compressionIndicator level bs in tiles
            | level <- levels
            ]
      mapM_ print tiless