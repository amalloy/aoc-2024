module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)

import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.Ix (inRange)

import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict qualified as M
import Data.Set qualified as S

import Linear.V2 (V2(..))

type Coord = V2 Int
type Frequency = Char

data Input = Input (Coord, Coord) (M.Map Frequency (S.Set Coord)) deriving Show

part1 :: Input -> Int
part1 (Input bounds freqs) = length . nubOrd . filter inBounds $ antinodes
  where inBounds = inRange bounds
        antinodes = do
          (_, antennas) <- M.assocs freqs
          let locs = toList antennas
          (a, b) <- (,) <$> locs <*> locs
          guard $ a > b
          let delta = a - b
          [a + delta, b - delta]

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare text = Input bounds antennas
  where bounds = (V2 1 1, V2 (length indexed) (length $ head indexed))
        antennas = M.fromListWith S.union $ do
          (pos, c) <- concat indexed
          case c of
            '.' -> []
            x | isAlphaNum x -> [(x, S.singleton pos)]
              | otherwise -> error $ "Unexpected character " <> [x]
        indexed :: [[(Coord, Char)]]
        indexed = zipWith (\y r -> zipWith (\x c -> (V2 y x, c)) [1..] r) [1..] (lines text)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
