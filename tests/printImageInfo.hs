import Control.Applicative ((<$>), pure)
import System.Environment (getArgs)
import Codec.Image.XCF
import qualified Data.Attoparsec as Attoparsec
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as CharString

main =
  head <$> getArgs >>=
  ByteString.readFile >>=
  pure . parse >>= \(Attoparsec.Done _ img) -> print img