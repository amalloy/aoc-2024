module Main where

import Control.Arrow ((&&&))

import Data.Array qualified as A
import Data.Set qualified as S

import Linear.V2 (V2(..), _y, perp)
import Linear.Vector (unit)

type Coord = V2 Int
newtype Direction = Direction Coord deriving (Eq, Ord, Show)
type Grid a = A.Array Coord a

north :: Direction
north = Direction (negate $ unit _y)
turnRight :: Direction -> Direction
turnRight (Direction d) = Direction (perp d)
move :: Direction -> Coord -> Coord
move (Direction d) c = c + d

data Entity = Empty | Obstructed deriving (Eq, Ord, Show)
data Guard = Guard Direction Coord deriving (Eq, Ord, Show)
data Input = Input Guard (Grid Entity) deriving (Eq, Ord, Show)

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare input = Input guard (A.array (V2 1 1, V2 (length $ head indexed) (length indexed))
                             (concat $ (map (map (fmap entity)) indexed)))
  where indexed = label (lines input)
        entity '.' = Empty
        entity '#' = Obstructed
        entity '^' = Empty
        entity x = error $ "Unexpected " ++ [x]
        label rows = zipWith (\y r -> zipWith (\x c -> (V2 x y, c)) [1..] r) [1..] rows
        guard = Guard north $ head [c | (c, '^') <- concat indexed]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
