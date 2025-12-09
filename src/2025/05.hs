{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldl')
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V

type Range = (Int, Int)

data Input = Input
  { ranges :: [Range],
    positions :: [Int]
  }
  deriving (Eq, Show)

-- $setup
-- >>> input = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> B.readFile "input/2025/05.txt"
  print $ solve input
  print $ partTwo input

-- >>> parse input
-- Input {ranges = [(3,5),(10,14),(16,20),(12,18)], positions = [1,5,8,11,17,32]}
parse :: B.ByteString -> Input
parse bs = Input (map parseRange rangeLines) (fst <$> mapMaybe B.readInt numLines)
  where
    ls = B.lines bs
    (rangeLines, _ : numLines) = break B.null ls

parseRange :: B.ByteString -> Range
parseRange bs = case B.split '-' bs of
  [B.readInt -> Just (a, _), B.readInt -> Just (b, _)] -> (a, b)

-- >>> solve example
-- 3
solve, partTwo :: Input -> Int
solve (Input rs xs) = length $ filter (isFresh merged) xs
  where
    merged = mergeRanges rs

-- >>> partTwo example
-- 14
partTwo (Input rs _) = sum [b + 1 - a | (a, b) <- mergeRanges rs]

-- >>> mergeRanges (ranges example)
-- [(3,5),(10,20)]
mergeRanges :: [Range] -> [Range]
mergeRanges [] = []
mergeRanges rs = reverse $ foldl' mergeStep [] (sort rs)
  where
    mergeStep :: [Range] -> Range -> [Range]
    mergeStep [] current = [current]
    mergeStep stack@((prevStart, prevEnd) : rest) current@(currStart, currEnd)
      -- Case 1: Gap exists, push new range onto the stack
      | currStart > prevEnd + 1 = current : stack
      -- Case 2: Overlap, update the top of the stack
      | otherwise = (prevStart, max prevEnd currEnd) : rest

-- >>> isFresh [(3,5)] 1
-- >>> isFresh [(3,5)] 5
-- False
-- True
isFresh :: [Range] -> Int -> Bool
isFresh (V.fromList -> rs) target = search 0 (V.length rs - 1)
  where
    search lo hi
      | lo > hi = False
      | otherwise =
          let mid = (lo + hi) `div` 2
              (start, end) = rs V.! mid
           in if target < start
                then search lo (mid - 1)
                else target <= end || search (mid + 1) hi
