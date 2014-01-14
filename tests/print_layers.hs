import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import Data.Word
import System.Environment (getArgs)
import Text.Printf (printf)

chunks :: Int -> BS.ByteString -> [BS.ByteString]
chunks n bs
  | BS.null bs = []
  | otherwise = (BS.take n bs):(chunks n $ BS.drop n bs)

showByteHex :: Word8 -> String
showByteHex w = printf "0x%02x" w

showChunkHex :: BS.ByteString -> String
showChunkHex = unwords . map showByteHex . BS.unpack

showBytesHex :: Int -> BS.ByteString -> String
showBytesHex n = unlines . map showChunkHex . chunks n

main :: IO ()
main = head <$> getArgs >>= BS.readFile >>= putStr . showBytesHex 10 . BS.take 100