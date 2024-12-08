module Main where

import Control.Arrow ((&&&))
import Control.Monad (join)

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

solve :: (Coord -> Coord -> [Coord]) -> Input -> Int
solve extend (Input bounds freqs) = length . nubOrd $ antinodes
  where antinodes = do
          (_, antennas) <- M.assocs freqs
          (a, b) <- filter (uncurry (/=)) . toList . join S.cartesianProduct $ antennas
          takeWhile (inRange bounds) (extend a b)

part1 :: Input -> Int
part1 = solve $ \a b -> [a + (a - b)]

part2 :: Input -> Int
part2 = solve $ \a b -> iterate (+ (a - b)) a

labelGrid :: String -> ((V2 Int, V2 Int), [(V2 Int, Char)])
labelGrid text = ( (V2 1 1, V2 (length rows) (length $ head rows))
                 , concat $ zipWith (\y r -> zipWith (\x c -> (V2 y x, c)) [1..] r) [1..] rows
                 )
  where rows = lines text

prepare :: String -> Input
prepare text = Input bounds antennas
  where (bounds, indexed) = labelGrid text
        antennas = M.fromListWith S.union $ do
          (pos, c) <- indexed
          case c of
            '.' -> []
            x | isAlphaNum x -> [(x, S.singleton pos)]
              | otherwise -> error $ "Unexpected character " <> [x]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
