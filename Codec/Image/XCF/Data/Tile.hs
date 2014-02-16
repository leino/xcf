module Codec.Image.XCF.Data.Tile
       (
         Run (..),
         Tiles (..),
         decodeTiles
       )
       where

import Data.Word
import qualified Data.ByteString as ByteString
import Prelude hiding (length)

data Run = Run Int Word8 | Block ByteString.ByteString deriving Show

data Tiles =
  RawTiles ByteString.ByteString | 
  RGBTiles [([Run], [Run], [Run])] |
  RGBAlphaTiles [([Run], [Run], [Run], [Run])] |
  GrayscaleTiles [[Run]] |
  GrayscaleAlphaTiles [([Run], [Run])] |
  IndexedTiles [[Run]] |
  IndexedAlphaTiles [([Run], [Run])] deriving Show

decodeRun :: Run -> ByteString.ByteString
decodeRun (Run n b) = ByteString.replicate n b
decodeRun (Block bs) = bs

intertwine :: [[Run]] -> ByteString.ByteString
intertwine = ByteString.concat . ByteString.transpose . map decodeTile1bpp

decodeTile1bpp :: [Run] -> ByteString.ByteString
decodeTile1bpp = ByteString.concat . map decodeRun

decodeTile2bpp :: ([Run], [Run]) -> ByteString.ByteString
decodeTile2bpp (runs1, runs2) = intertwine [runs1, runs2]

decodeTile3bpp :: ([Run], [Run], [Run]) -> ByteString.ByteString
decodeTile3bpp (runs1, runs2, runs3) = intertwine [runs1, runs2, runs3]

decodeTile4bpp :: ([Run], [Run], [Run], [Run]) -> ByteString.ByteString
decodeTile4bpp (runs1, runs2, runs3, runs4) = intertwine [runs1, runs2, runs3, runs4]

decodeTiles :: Tiles -> ByteString.ByteString
decodeTiles (RawTiles bs) = bs
decodeTiles (RGBTiles rgbRuns) = ByteString.concat $ map decodeTile3bpp rgbRuns
decodeTiles (RGBAlphaTiles rgbaRuns) = ByteString.concat $ map decodeTile4bpp rgbaRuns
decodeTiles (GrayscaleTiles wRuns) = ByteString.concat $ map decodeTile1bpp wRuns
decodeTiles (GrayscaleAlphaTiles waRuns) = ByteString.concat $ map decodeTile2bpp waRuns
decodeTiles (IndexedTiles iRuns) = ByteString.concat $ map decodeTile1bpp iRuns
decodeTiles (IndexedAlphaTiles iaRuns) = ByteString.concat $ map decodeTile2bpp iaRuns
