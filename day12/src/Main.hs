module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Maybe (maybeToList)
import Linear.V2 (V2(..))
import Linear.Vector (basis)

import qualified Data.Array.Unboxed as A
import qualified Data.Set as S

type Coord = V2 Int
type Garden = A.UArray Coord Char
type Input = Garden

neighbors :: Num a => V2 a -> [V2 a]
neighbors p = (+ p) <$> ([id, negate] <*> basis)

floodFill :: Ord v => (v -> [v]) -> v -> S.Set v
floodFill edges s = go S.empty (S.singleton s)
  where go seen frontier = case S.minView frontier of
          Nothing -> seen
          Just (v, f) | v `S.member` seen -> go seen f
                      | otherwise -> go (S.insert v seen) newFrontier
            where newFrontier = f <> S.fromList (edges v) `S.difference` seen

area :: S.Set a -> Int
area = S.size

perimeter :: (Ord a, Num a) => S.Set (V2 a) -> Int
perimeter region = sum . map fencesNeeded . S.toList $ region
  where fencesNeeded = length . filter (`S.notMember` region) . neighbors

part1 :: Input -> Int
part1 = sum . map fenceCost . allRegions
  where fenceCost = liftA2 (*) area perimeter . S.map fst
        allRegions :: A.UArray (V2 Int) Char -> [S.Set (V2 Int, Char)]
        allRegions g = go (S.fromList (A.assocs g))
          where go pending = case S.minView pending of
                  Nothing -> []
                  Just (plot, pending') -> newRegion : go (pending' `S.difference` newRegion)
                    where newRegion = floodFill connected plot
                          connected (coord, p) = do
                            coord' <- neighbors coord
                            p' <- maybeToList $ g A.!? coord'
                            guard $ p == p'
                            pure (coord', p')

part2 :: Input -> ()
part2 = const ()

labelGrid :: String -> ((V2 Int, V2 Int), [(V2 Int, Char)])
labelGrid text = ( (V2 1 1, V2 (length rows) (length $ head rows))
                 , concat $ zipWith (\y r -> zipWith (\x c -> (V2 y x, c)) [1..] r) [1..] rows
                 )
  where rows = lines text

prepare :: String -> Input
prepare input = A.array bounds plots
  where (bounds, plots) = labelGrid input

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
