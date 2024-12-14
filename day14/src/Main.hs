module Main where

import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)

import qualified Data.Map as M

import Text.Regex.Applicative (match, string, many)
import Text.Regex.Applicative.Common (signed, decimal)

type Coord a = (a, a)
type RoomSize a = (a, a)
data Robot a = Robot {position, velocity :: Coord a} deriving Show
data ModProblem a = ModProblem {startPos, stepSize, roomSize, numSteps :: a} deriving Show
type Input = [Robot Int]

finalPosition1d :: Integral a => ModProblem a -> a
finalPosition1d (ModProblem p v s n) = (p + v * n) `mod` s

finalPosition2d :: Integral a => RoomSize a -> a -> Robot a -> Coord a
finalPosition2d (w, h) n (Robot (x, y) (dx, dy)) =
  ( finalPosition1d (ModProblem x dx w n)
  , finalPosition1d (ModProblem y dy h n)
  )

inFirstHalf :: Integral a => a -> a -> Maybe Bool
inFirstHalf size pos = case compare pos (size `div` 2) of
  EQ -> Nothing
  LT -> Just True
  GT -> Just False

quadrant :: Integral a => RoomSize a -> Coord a -> Maybe (Bool, Bool)
quadrant (w, h) (x, y) = (,) <$> inFirstHalf w x <*> inFirstHalf h y

part1 :: Input -> Int
part1 = product . freqs . mapMaybe (quadrant size . finalPosition2d size 100)
  where freqs = M.fromListWith (+) . map (,1)
        size = (101, 103)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = maybe (error "no parse") id . match input
  where input = many (robot <* string "\n")
        robot = Robot <$> (coord "p" <* string " ") <*> coord "v"
        coord label = (,)
          <$> (string (label <> "=") *> signed decimal <* string ",")
          <*> signed decimal

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
