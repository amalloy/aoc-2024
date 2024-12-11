module Main where

import Control.Arrow ((&&&))

import Data.MemoTrie (memo)

import qualified Data.IntMap as M

type Label = Int
type Quantity = Int
type StoneCounts = M.IntMap Quantity
type Input = StoneCounts

freqs :: [Label] -> StoneCounts
freqs = M.fromListWith (+) . map (, 1)

step :: Label -> StoneCounts
step 0 = M.singleton 1 1
step n | even len = let (a, b) = splitAt (len `div` 2) s
                    in freqs [read a, read b]
       | otherwise = M.singleton (n * 2024) 1
  where s = show n
        len = length s

blink :: StoneCounts -> StoneCounts
blink = \m -> M.unionsWith (+) $ do
  (label, count) <- M.assocs m
  pure (fmap (* count) (step' label))
  where step' = memo step

solve :: Int -> Input -> Int
solve n = sum . M.elems . (!! n) . iterate blink

part1 :: Input -> Int
part1 = solve 25

part2 :: Input -> Int
part2 = solve 75

prepare :: String -> Input
prepare = freqs . map read . words

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
