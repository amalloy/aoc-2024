{-# LANGUAGE LambdaCase, PartialTypeSignatures #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard, when)
import Control.Monad.ST (ST, runST)
import Data.Array (assocs)
import Data.Array.ST (STArray, newArray, newListArray, runSTArray, readArray, writeArray, modifyArray')
import Data.Char (digitToInt, isDigit)
import Data.Foldable (sequenceA_)
import Data.List (mapAccumL, sortOn)
import Data.Maybe (catMaybes)

import qualified Data.Heap as H

data Block = Free | File Int deriving Show
data Chunk = Chunk Int Block deriving Show
data FileData = FileData {_id, _len, _fileNum :: Int} deriving Show

defragment :: [Block] -> [(Int, Block)]
defragment blocks = assocs $ runSTArray $ do
  let bounds = (0, length blocks - 1)
  disk <- newListArray bounds blocks
  let go p q | p >= q = pure disk
             | otherwise = ((,) <$> readArray disk p <*> readArray disk q) >>= \case
                 (File _, _) -> go (p + 1) q
                 (_, Free) -> go p (q - 1)
                 (Free, File f) -> writeArray disk p (File f)
                                   *> writeArray disk q Free
                                   *> go (p + 1) (q - 1)
  uncurry go bounds

part1 :: String -> Int
part1 = checksum . defragment . prepare (\len b bs -> replicate len b <> bs)
  where checksum = sum . map score
        score (_, Free) = 0
        score (pos, (File num)) = pos * num

indexBlocks :: [Chunk] -> [(Int, Chunk)]
indexBlocks = snd . mapAccumL go 0
  where go ix c@(Chunk len _) = (ix + len, (ix, c))

part2 :: String -> Int
part2 input = runST $ do
  let chunks = prepare (\len b bs -> Chunk len b : bs) input
      blocks = indexBlocks chunks
      inputFiles = reverse [FileData ix len num | (ix, Chunk len (File num)) <- blocks]
  freeHeaps <- newArray (1, 9) H.empty :: ST s (STArray s Int (H.Heap Int))
  sequenceA_ $ do
    (ix, Chunk len Free) <- blocks
    guard $ len >= 1
    pure $ modifyArray' freeHeaps len (H.insert ix)
  let firstFreeBlock :: Int -> ST _ (Maybe (Int, (Int, (H.Heap Int))))
      firstFreeBlock size = fmap (size,) . H.viewMin <$> readArray freeHeaps size
      moveLeft orig@(FileData ix len num) = do
        firsts <- catMaybes <$> traverse firstFreeBlock [len..9]
        case sortOn (fst . snd) . filter ((< ix) . fst . snd) $ firsts of
          [] -> pure orig
          ((freeSize, (freeIx, h')):_) -> do
            writeArray freeHeaps freeSize h'
            when (freeSize > len) $ modifyArray' freeHeaps (freeSize - len) (H.insert (freeIx + len))
            pure $ FileData freeIx len num
  sum . map score <$> traverse moveLeft inputFiles

score :: FileData -> Int
score (FileData ix len file) = file * (sum [ix..ix+len-1])

prepare :: (Int -> Block -> [a] -> [a]) -> String -> [a]
prepare f = file 0 . map digitToInt . filter isDigit
  where file _ [] = []
        file n (size:rest) = f size (File n) $ empty n rest
        empty _ [] = []
        empty n (size:rest) = f size Free $ file (n + 1) rest

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2)
