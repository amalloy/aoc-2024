module Main where

import Control.Arrow ((&&&))

import Data.List (tails, transpose, isPrefixOf)

type Input = [String]

transpositions :: [[a]] -> [[[a]]]
transpositions = take 4 . iterate (reverse . transpose)

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals arr@(_:more) = transpose (zipWith drop [0..] arr) ++ diagonals more

searchGrids :: [[a]] -> [[a]]
searchGrids xs = do
  grid <- transpositions xs
  orientation <- [tails =<< grid, diagonals grid]
  orientation

part1 :: Input -> Int
part1 = length . filter ("XMAS" `isPrefixOf`) . searchGrids

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
