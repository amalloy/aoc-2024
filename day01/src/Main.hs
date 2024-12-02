module Main where

import Control.Arrow ((&&&))

import Data.List (sort)

type Input = [(Int, Int)]

part1 :: Input -> Int
part1 = sum . map abs . uncurry (zipWith (-)) . sortEach . unzip
  where sortEach (xs, ys) = (sort xs, sort ys)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map (line . words) . lines
  where line [x, y] = (read x, read y)
        line invalid = error (show invalid)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
