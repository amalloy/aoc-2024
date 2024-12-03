module Main where

import Control.Arrow ((&&&))

type Level = Int
type Report = [Level]
type Input = [Report]

safe :: Report -> Bool
safe = liftA2 (&&) sameSign (all small) . deltas
  where sameSign [] = True
        sameSign (x:xs) = all (== signum x) $ map signum xs
        deltas = zipWith (-) <*> tail
        small = liftA2 (&&) (>= 1) (<= 3) . abs

part1 :: Input -> Int
part1 = length . filter safe

part2 :: Input -> Int
part2 = length . filter canBeMadeSafe
  where canBeMadeSafe = any safe . removeUpTo 1
        removeUpTo :: Int -> [a] -> [[a]]
        removeUpTo 0 xs = [xs]
        removeUpTo _ [] = [[]]
        removeUpTo n (x:xs) = ((x:) <$> removeUpTo n xs) <> removeUpTo (n - 1) xs

prepare :: String -> Input
prepare = map (map read . words) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
