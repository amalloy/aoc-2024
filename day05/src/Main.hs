module Main where

import Control.Applicative (Alternative, many)
import Control.Arrow ((&&&))

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Text.Regex.Applicative (RE, match, sym)
import Text.Regex.Applicative.Common (decimal)

type Page = Int
data Dependency = Dependency {before, after :: Page} deriving Show
type RuleOrdering = M.Map Page (S.Set Page)
type Update = [Page]

data Input = Input RuleOrdering [Update] deriving Show

sepBy1 :: Alternative f => f a -> f b -> f [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

input :: RE Char Input
input = Input <$> (ruleOrdering <* sym '\n') <*> many update
  where ruleOrdering = (M.fromListWith (<>) . map toKv) <$> many dependency
        dependency = Dependency <$> (decimal <* sym '|') <*> (decimal <* sym '\n')
        toKv (Dependency before after) = (after, S.singleton before)
        update = (decimal `sepBy1` sym ',') <* sym '\n'

part1 :: Input -> Int
part1 (Input ordering updates) = sum . map middlePage . filter valid $ updates
  where middlePage xs = xs !! (length xs `div` 2)
        valid [] = True
        valid (p:ps) | any (`S.member` (M.findWithDefault mempty p ordering)) ps = False
                     | otherwise = valid ps

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = maybe (error "no parse") id . match input

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
