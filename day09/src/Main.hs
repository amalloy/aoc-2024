{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Array (assocs)
import Data.Array.ST (newListArray, runSTArray, readArray, writeArray)
import Data.Char (digitToInt, isDigit)

data Block = Free | File Int deriving Show

type Input = [Block]

defragment :: [Block] -> [(Int, Block)]
defragment blocks = assocs $ runSTArray $ do
  let bounds = (0, length blocks - 1)
  disk <- newListArray bounds blocks
  let go p q | p >= q = pure disk
             | otherwise = ((,) <$> readArray disk p <*> readArray disk q) >>= \case
                 (File _, _) -> go (p + 1) q
                 (_, Free) -> go p (q - 1)
                 (Free, File f) -> writeArray disk p (File f)
                                   *> writeArray disk q Free
                                   *> go (p + 1) (q - 1)
  uncurry go bounds

part1 :: Input -> Int
part1 = checksum . defragment
  where checksum = sum . map score
        score (_, Free) = 0
        score (pos, (File num)) = pos * num

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = file 0 . map digitToInt . filter isDigit
  where file _ [] = []
        file n (size:rest) = replicate size (File n) <> empty n rest
        empty _ [] = []
        empty n (size:rest) = replicate size Free <> file (n + 1) rest

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
