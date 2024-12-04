module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)

import Data.List (tails, transpose, isPrefixOf, sort)

type Input = [String]

rotations :: [[a]] -> [[[a]]]
rotations = take 4 . iterate (reverse . transpose)

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals arr@(_:more) = transpose (zipWith drop [0..] arr) ++ diagonals more

searchGrids :: [[a]] -> [[a]]
searchGrids xs = do
  grid <- rotations xs
  orientation <- [tails =<< grid, diagonals grid]
  orientation

part1 :: Input -> Int
part1 = length . filter ("XMAS" `isPrefixOf`) . searchGrids

subMatricesOfSize :: Int -> [[a]] -> [[[a]]]
subMatricesOfSize n xs = do
  grid <- tails xs
  let window = take n grid
  guard $ length window == n
  let lens = map (map (take n) . tails) window
  transpose lens

part2 :: Input -> Int
part2 = length . filter isXmas . subMatricesOfSize 3
  where isXmas [[a, _, b], [_, 'A', _], [c, _, d]] = sort [a, d] == "MS" && sort [b, c] == "MS"
        isXmas _ = False

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
