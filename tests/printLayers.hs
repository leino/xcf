import Control.Applicative ((<$>), pure)
import System.Environment (getArgs)
import Codec.Image.XCF
import Codec.Image.XCF.Data.Image
import qualified Data.Attoparsec as Attoparsec
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as CharString

main =
  head <$> getArgs >>= ByteString.readFile >>= \bs -> do
    (pure $ parse bs) >>=  \(Attoparsec.Done _ img) -> do
      mapM_ print [let (Attoparsec.Done _ layer) = parseLayerAt lp bs in layer
                  | lp <- layerPointers img]