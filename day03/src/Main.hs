module Main where

import Control.Applicative (asum, (<|>))
import Control.Arrow ((&&&))
import Control.Monad (replicateM)

import Data.Char (isDigit)
import Data.List (mapAccumL, unfoldr)

import Text.Regex.Applicative (RE, psym, string, findFirstInfix)

type Input = String

data Mul = Mul Int Int deriving (Show, Eq, Ord)
data Instruction = Do | Dont | MulInst Mul deriving (Show, Eq, Ord)

op :: String -> RE Char a -> RE Char a
op name r = string name *> string "(" *> r <* string ")"

mul :: RE Char Mul
mul = uncurry Mul <$> op "mul" ((,) <$> (decimal <* string ",") <*> decimal)
  where decimal = read <$> asum [replicateM n (psym isDigit) | n <- [1..3]]
instruction :: RE Char Instruction
instruction = (Do <$ op "do" (pure ()))
          <|> (Dont <$ op "don't" (pure ()))
          <|> (MulInst <$> mul)

runMul :: Mul -> Int
runMul (Mul x y) = x * y

findAll :: RE Char a -> String -> [a]
findAll re = unfoldr go
  where go s = fmap (\(_, x, r) -> (x, r)) (findFirstInfix re s)

part1 :: Input -> Int
part1 = sum . map runMul . findAll mul

data Mode = Enabled | Disabled deriving Eq

part2 :: Input -> Int
part2 = sum . snd . mapAccumL go Enabled . findAll instruction
  where go _s Do = (Enabled, 0)
        go _s Dont = (Disabled, 0)
        go s (MulInst m) = (s, case s of
                                 Enabled -> runMul m
                                 Disabled -> 0)

prepare :: String -> Input
prepare = id

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
