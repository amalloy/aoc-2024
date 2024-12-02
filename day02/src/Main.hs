module Main where

import Control.Arrow ((&&&))

type Level = Int
type Record = [Level]
type Input = [Record]

part1 :: Input -> Int
part1 = length . filter safe
  where safe = liftA2 (&&) sameSign (all small) . deltas
        sameSign [] = True
        sameSign (x:xs) = all (== signum x) $ map signum xs
        deltas = zipWith (-) <*> tail
        small = liftA2 (&&) (>= 1) (<= 3) . abs

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map (map read . words) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
