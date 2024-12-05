module Main where

import Control.Applicative (Alternative, many)
import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)

import Data.Graph (reverseTopSort, graphFromEdges)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Text.Regex.Applicative (RE, match, sym)
import Text.Regex.Applicative.Common (decimal)

type Page = Int
data Dependency = Dependency {before, after :: Page} deriving Show
type Update = [Page]

data Input = Input (M.Map Page (S.Set Page)) [Update] deriving Show

sepBy1 :: Alternative f => f a -> f b -> f [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

input :: RE Char Input
input = Input <$> (ruleOrdering <* sym '\n') <*> many update
  where ruleOrdering = (M.fromListWith (<>) . map toKv) <$> many dependency
        dependency = Dependency <$> (decimal <* sym '|') <*> (decimal <* sym '\n')
        toKv (Dependency before after) = (after, S.singleton before)
        update = (decimal `sepBy1` sym ',') <* sym '\n'

middlePage :: [a] -> a
middlePage xs = xs !! (length xs `div` 2)

part1 :: Input -> Int
part1 (Input ordering updates) = sum . map middlePage . filter valid $ updates
  where valid [] = True
        valid (p:ps) | any (`S.member` (M.findWithDefault mempty p ordering)) ps = False
                     | otherwise = valid ps

part2 :: Input -> Int
part2 (Input ordering updates) = sum . map middlePage . mapMaybe reorder $ updates
  where reorder pages | pages == correctOrder = Nothing
                      | otherwise = Just correctOrder
          where correctOrder = pageFromVertex <$> reverseTopSort graph
                (graph, getNode, _) = graphFromEdges [(p, p, deps p) | p <- pages]
                pageFromVertex v = let (p, _, _) = getNode v in p
                queue = S.fromList pages
                deps p = S.toList $ S.intersection queue (M.findWithDefault mempty p ordering)

prepare :: String -> Input
prepare = maybe (error "no parse") id . match input

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
