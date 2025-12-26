{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import qualified Data.ByteString.Char8 as B
import Data.Char (digitToInt)
import Data.Function (fix)
import Data.List (foldl1')

-- $setup
-- >>> input = ["987654321111111", "811111111111119", "234234234234278", "818181911112111"]

main :: IO ()
main = do
  input <- B.lines <$> B.readFile "input/2025/03.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve input
-- 357
solve, partTwo :: [B.ByteString] -> Int
solve = sum . map maxJoltage
partTwo = sum . map (largestSubseq 12)

-- >>> map maxJoltage input
-- [98,89,78,92]
maxJoltage :: B.ByteString -> Int
maxJoltage = snd . B.foldl' go (-1, 0)
  where
    go (!bestLeft, !bestJ) (digitToInt -> d) =
      (max bestLeft d, max bestJ (10 * bestLeft + d))

-- >>> map (largestSubseq 12) input
-- [987654321111,811111111119,434234234278,888911112111]
largestSubseq :: Int -> B.ByteString -> Int
largestSubseq k = foldl1' (\a d -> a * 10 + d) . take k . reverse . go 0 []
  where
    -- len: Current depth of the stack (tracked explicitly to avoid O(N) length calls)
    -- stk: The stack of selected digits (head is the most recently added)
    go !len stk b = case B.uncons b of
      Nothing -> stk
      Just (digitToInt -> d, ds) ->
        let remaining = B.length ds

            -- Inner loop: Pop elements from the stack if:
            -- 1. The stack top (x) is smaller than current digit (d)
            -- 2. We have enough remaining digits to still fill the request of size 'k'
            (len', stk') =
              fix
                ( \pop !l -> \case
                    (x : xs) | x < d && l + remaining >= k -> pop (l - 1) xs
                    s -> (l, s)
                )
                len
                stk
         in go (len' + 1) (d : stk') ds
