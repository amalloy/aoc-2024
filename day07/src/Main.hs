module Main where

import Control.Arrow ((&&&))

import Text.Regex.Applicative (match, sym, many)
import Text.Regex.Applicative.Common (decimal)

data Equation a = Equation a [a] deriving Show
data Solution a = Solution a a [(Operator, a)] deriving Show
data Operator = Plus | Times deriving (Show, Enum, Bounded)
type Input = [Equation Integer]

apply :: Num a => Operator -> a -> a -> a
apply Plus = (+)
apply Times = (*)

solutions :: (Num a, Ord a) => [Operator] -> Equation a -> [Solution a]
solutions ops (Equation goal (num:nums)) = Solution goal num <$> go num nums
  where go total atoms = case compare total goal of
          GT -> []
          EQ | null atoms -> [[]]
          _ -> case atoms of
                 [] -> []
                 x:xs -> do
                   op <- ops
                   ((op, x) :) <$> go (apply op total x) xs

part1 :: Input -> Integer
part1 = sum . map score
  where score eq@(Equation goal _) | null (solutions [Times, Plus] eq) = 0
                                   | otherwise = goal

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = maybe (error "no parse") id . match input
  where input = many (equation <* sym '\n')
        equation = Equation <$> (decimal <* sym ':') <*> (many (sym ' ' *> decimal))

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
