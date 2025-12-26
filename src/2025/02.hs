{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S

type Range = (Integer, Integer)

-- $setup
-- >>> input = B.split ',' "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
-- >>> example = map parse input

main :: IO ()
main = do
  input <- map parse . B.split ',' <$> B.readFile "input/2025/02.txt"
  print $ solve input
  print $ partTwo input

-- >>> parse "11-22"
-- (11,22)
parse :: B.ByteString -> Range
parse bs = (a, b)
  where
    [B.readInteger -> Just (a, _), B.readInteger -> Just (b, _)] = B.split '-' bs

-- >>> digits 123
-- 3
digits :: Integer -> Int
digits = go 0
  where
    go !c !x
      | x < 0 = go c (-x)
      | x < 10 = c + 1
      | otherwise = go (c + 1) (x `div` 10)

-- >>> solve example
-- >>> partTwo example
-- 1227775554
-- 4174379265
solve, partTwo :: [Range] -> Integer
solve = sum . map sumDoubled
partTwo = sum . map sumRepeats

-- | Sum all doubled numbers in a single range.
sumDoubled :: Range -> Integer
sumDoubled (lo, hi) = sum [l | m <- [1 .. digits hi `div` 2], l <- validIDsFor lo hi m 2]

-- | Sum all doubled numbers in a single range, considering ALL repeats (k >= 2).
sumRepeats :: Range -> Integer
sumRepeats (lo, hi) =
  sum $ S.fromList [l | m <- [1 .. digits hi `div` 2], k <- [2 .. digits hi `div` m], l <- validIDsFor lo hi m k]

-- | Generate all repeated IDs L * factor within [lo, hi] for given m (base length) and k (repeats)
-- >>> validIDsFor 11 22 1 2
-- >>> validIDsFor 95 115 1 2
-- >>> validIDsFor 95 115 1 3
-- [11,22]
-- [99]
-- [111]
validIDsFor :: Integer -> Integer -> Int -> Int -> [Integer]
validIDsFor lo hi m k = [l * factor | l <- [start .. end]]
  where
    factor = (10 ^ (k * m) - 1) `div` (10 ^ m - 1)
    minL = 10 ^ m `div` 10
    maxL = 10 ^ m - 1
    start = max minL ((lo + factor - 1) `div` factor) -- ceil(lo / factor)
    end = min maxL (hi `div` factor) -- floor(hi / factor)
