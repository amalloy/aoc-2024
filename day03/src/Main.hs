module Main where

import Control.Applicative (asum)
import Control.Arrow ((&&&))
import Control.Monad (replicateM)

import Data.Char (isDigit)
import Data.List (unfoldr)

import Text.Regex.Applicative (RE, psym, string, findFirstInfix)

type Input = String

data Mul = Mul Int Int deriving (Show, Eq, Ord)

mul :: RE Char Mul
mul = Mul <$> (string "mul(" *> decimal) <*> (string "," *> decimal <* string ")")
  where decimal = read <$> asum [replicateM n (psym isDigit) | n <- [1..3]]

runMul :: Mul -> Int
runMul (Mul x y) = x * y

findAll :: RE Char a -> String -> [a]
findAll re = unfoldr go
  where go s = fmap (\(_, x, r) -> (x, r)) (findFirstInfix re s)

part1 :: Input -> Int
part1 = sum . map runMul . findAll mul

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = id

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
