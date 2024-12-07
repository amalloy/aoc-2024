module Main where

import Control.Arrow ((&&&))

import Data.List.NonEmpty (NonEmpty(..), some1)

import Text.Regex.Applicative (match, sym, many)
import Text.Regex.Applicative.Common (decimal)

data Equation a = Equation a (NonEmpty a) deriving Show
data Solution a = Solution a a [(Operator, a)] deriving Show
data Operator = Plus | Times | Cat deriving (Show, Enum, Bounded)
type Input = [Equation Int]

apply :: (Read a, Show a, Num a) => Operator -> a -> a -> a
apply Plus = (+)
apply Times = (*)
apply Cat = \a b -> read (show a <> show b)

solutions :: (Read a, Show a, Num a, Ord a) => [Operator] -> Equation a -> [Solution a]
solutions ops (Equation goal (num :| nums)) = Solution goal num <$> go num nums
  where go total atoms = case (compare total goal, atoms) of
          (GT, _) -> []
          (LT, []) -> []
          (EQ, []) -> [[]]
          (_, (x:xs)) -> do
            op <- ops
            ((op, x) :) <$> go (apply op total x) xs

solve :: [Operator] -> Input -> Int
solve ops = sum . map score
  where score eq@(Equation goal _) | null (solutions ops eq) = 0
                                   | otherwise = goal
part1 :: Input -> Int
part1 = solve [Times, Plus]

part2 :: Input -> Int
part2 = solve [Times, Plus, Cat]

prepare :: String -> Input
prepare = maybe (error "no parse") id . match input
  where input = many (equation <* sym '\n')
        equation = Equation <$> (decimal <* sym ':') <*> (some1 (sym ' ' *> decimal))

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
