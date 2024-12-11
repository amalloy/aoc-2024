module Main where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)

import qualified Data.IntMap as M

type Label = Int
type Quantity = Int
type StoneCounts = M.IntMap Quantity
type Input = StoneCounts

freqs :: [Label] -> StoneCounts
freqs = M.fromListWith (+) . map (, 1)

split :: Label -> Maybe (Label, Label)
split n | even len = Just $ bimap read read (splitAt (len `div` 2) s)
        | otherwise = Nothing
  where s = show n
        len = length s

step :: Label -> StoneCounts
step 0 = M.singleton 1 1
step n = case split n of
  Just (a, b) -> freqs [a, b]
  Nothing -> M.singleton (n * 2024) 1

blink :: StoneCounts -> StoneCounts
blink m = M.unionsWith (+) $ do
  (label, count) <- M.assocs m
  pure (fmap (* count) (step label))

part1 :: Input -> Int
part1 = sum . M.elems . (!! 25) . iterate blink

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = freqs . map read . words

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
