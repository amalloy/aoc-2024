module Main where

import Control.Arrow ((&&&))
import Data.Array (assocs)
import Data.Array.ST (newListArray, runSTArray, readArray, writeArray)
import Data.Char (digitToInt, isDigit)

data Block = Free | File Int deriving Show

type Input = [Block]

part1 :: Input -> ()
part1 = const ()

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
