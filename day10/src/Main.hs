module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)

import Data.Char (digitToInt)

import qualified Data.Array as A
import qualified Data.Set as S

import Linear.V2 (V2(..))
import Linear.Vector (basis)

type Height = Int
type Input = A.Array (V2 Int) Height

part1 :: Input -> Int
part1 arr = sum . map score . A.assocs $ reachableSummits
  where score (pos, summits) | arr A.! pos == 0 = S.size summits
                             | otherwise = 0
        reachableSummits = A.array bounds $ do
          (pos, height) <- A.assocs arr
          pure (pos, if height == 9
                 then S.singleton pos
                 else mconcat $ do
                   pos' <- (pos +) <$> ([id, negate] <*> basis)
                   guard $ A.inRange bounds pos'
                   guard $ arr A.! pos' == height + 1
                   pure $ reachableSummits A.! pos')
        bounds = (A.bounds arr)

part2 :: Input -> ()
part2 = const ()

labelGrid :: String -> ((V2 Int, V2 Int), [(V2 Int, Char)])
labelGrid text = ( (V2 1 1, V2 (length rows) (length $ head rows))
                 , concat $ zipWith (\y r -> zipWith (\x c -> (V2 y x, c)) [1..] r) [1..] rows
                 )
  where rows = lines text

prepare :: String -> Input
prepare = go . labelGrid
  where go (bounds, heights) = A.array bounds $ map (fmap digitToInt) heights

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
