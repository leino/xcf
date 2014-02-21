module Codec.Image.XCF.Data.Tile
       (
         Run (..),
         Tiles (..),
         decodeTiles,
         tileSizes
       )
       where

import Data.Word
import Data.List (transpose)
import qualified Data.ByteString as ByteString
import Prelude hiding (length)

-- widht and height of a tile, in pixels
tileDimension = 64

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

decodeTiles :: Int -> Int -> Tiles -> ByteString.ByteString
decodeTiles levelWidth levelHeight (RawTiles bs) = bs
decodeTiles levelWidth levelHeight (RGBTiles rgbRuns) =
  deinterlaceTiles 3 levelWidth levelHeight $ map decodeTile3bpp rgbRuns
decodeTiles levelWidth levelHeight (RGBAlphaTiles rgbaRuns) =
  deinterlaceTiles 4 levelWidth levelHeight $ map decodeTile4bpp rgbaRuns
decodeTiles levelWidth levelHeight (GrayscaleTiles wRuns) =
  deinterlaceTiles 1 levelWidth levelHeight $ map decodeTile1bpp wRuns
decodeTiles levelWidth levelHeight (GrayscaleAlphaTiles waRuns) =
  deinterlaceTiles 2 levelWidth levelHeight $ map decodeTile2bpp waRuns
decodeTiles levelWidth levelHeight (IndexedTiles iRuns) =
  deinterlaceTiles 1 levelWidth levelHeight $ map decodeTile1bpp iRuns
decodeTiles levelWidth levelHeight (IndexedAlphaTiles iaRuns) =
  deinterlaceTiles 2 levelWidth levelHeight $ map decodeTile2bpp iaRuns

deinterlaceTiles bpp levelWidth levelHeight =
  ByteString.pack . deinterlace bpp levelWidth levelHeight . map ByteString.unpack

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs):(chunksOf n $ drop n xs)

deinterlace :: Int -> Int -> Int -> [[a]] -> [a]
deinterlace bpp levelWidth levelHeight ts =
  let splitTiles = [chunksOf (w*bpp) t -- split the tiles into lists of rows pixels
                   | (w,t) <- zip (map fst $ tileSizes levelWidth levelHeight) ts]
      numCols = case divMod levelWidth tileDimension of
        (q,0) -> q
        (q,_) -> q + 1
      rows = chunksOf numCols splitTiles         -- split the split tiles into rows of tiles
      deinterlacedRows = map (concat . map concat . transpose) rows
      in
  concat deinterlacedRows

-- make n in chunks of tileDimension (64 px), plus possibly a smaller remainder chunk
chunks n = case divMod n tileDimension of (q,0) -> replicate q tileDimension
                                          (q,r) -> replicate q tileDimension ++ [r]


-- Generate a sequence of tile sizes for a level.
-- The sequence is in row-major, top-to-bottom and left-to-right order.
tileSizes :: Int -> Int -> [(Int, Int)]
tileSizes levelWidth levelHeight =
  [(w,h) | h <- chunks levelHeight, w <- chunks levelWidth]