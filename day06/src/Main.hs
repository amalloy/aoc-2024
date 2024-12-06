module Main where

import Control.Arrow ((&&&))
import Control.Monad qualified

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
data Guard = Guard {face :: Direction, pos :: Coord} deriving (Eq, Ord, Show)
data Input = Input Guard (Grid Entity) deriving (Eq, Ord, Show)

inBounds :: Grid a -> Coord -> Bool
inBounds = A.inRange . A.bounds

step :: Grid Entity -> Guard -> Guard
step grid (Guard d p) = Guard d' (move d' p)
  where d' = head . filter walkable . iterate turnRight $ d
        walkable candidateDir = not ((inBounds grid p') && (grid A.! p' == Obstructed))
          where p' = move candidateDir p

part1 :: Input -> Int
part1 (Input guard grid) =
  S.size
  . S.fromList
  . takeWhile (inBounds grid)
  . map pos
  . iterate (step grid)
  $ guard

part2 :: Input -> Int
part2 (Input guard grid) = length . filter id $ do
  c <- A.indices grid
  Control.Monad.guard (c /= (pos guard))
  let grid' = grid A.// [(c, Obstructed)]
  return $ infiniteLoop grid'
  where infiniteLoop g = go S.empty (takeWhile (inBounds g . pos)
                                     (iterate (step g) guard))
        go _ [] = False
        go seen (g:gs) | S.member g seen = True
                       | otherwise = go (S.insert g seen) gs

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
