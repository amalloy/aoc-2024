module Main where

import Control.Arrow ((&&&))

import Data.Maybe (mapMaybe)

import Text.Regex.Applicative (match, string, many, optional)
import Text.Regex.Applicative.Common (decimal)

type Coord = (Int, Int)
type Button = Coord
data Machine = Machine Button Button Coord deriving Show

det :: Int -> Int -> Int -> Int -> Int
det a b c d = a*d - b*c

win :: Machine -> Maybe Coord
win (Machine (ax, ay) (bx, by) (gx, gy))
  | d == 0 = Nothing
  | otherwise = case (divMod da d, divMod db d) of
      ((a, 0), (b, 0)) -> Just (a, b)
      _ -> Nothing
  where d = det ax bx ay by
        da = det gx bx gy by
        db = det ax gx ay gy

type Input = [Machine]

solve :: Input -> Int
solve = sum . map score . mapMaybe win
  where score (a, b) = 3*a + b

part1 :: Input -> Int
part1 = solve

part2 :: Input -> Int
part2 = solve . map (shiftGoalposts 10000000000000)
  where shiftGoalposts amt (Machine a b (x, y)) = Machine a b (x + amt, y + amt)

prepare :: String -> Input
prepare = maybe (error "no parse") id . match input
  where input = many (machine <* (optional $ string "\n"))
        machine = Machine <$> line "Button A" "+" <*> line "Button B" "+" <*> line "Prize" "="
        line label op = (,) <$>
          (string (label <> ": X" <> op) *> decimal) <*>
          (string (", Y" <> op) *> decimal <* string "\n")

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
