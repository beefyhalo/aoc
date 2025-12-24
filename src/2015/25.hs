{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Mod (Mod (..), (^%))

-- The modulus as a type-level natura
startCode, multiplier :: Mod 33554393
startCode = 20151125
multiplier = 252533

-- $setup
-- >>> input = "To continue, please consult the code grid in the manual.  Enter the code at row 2, column 1."
-- >>> (exampleRow, exampleCol) = parse input

main :: IO ()
main = do
  (targetRow, targetCol) <- parse <$> readFile "input/2015/25.txt"
  print $ solve targetRow targetCol

parse :: String -> (Int, Int)
parse s = (r, c)
  where
    [r, c] = read . filter isDigit <$> splitOn ", column " s

-- >>> solve exampleRow exampleCol
-- 31916031
solve :: Int -> Int -> Int
solve row col = fromIntegral . unMod $ startCode * multiplier ^% seqIndex row col

-- solve row col = codes !! seqIndex row col
--   where codes = iterate (\prev -> (prev * multiplier) `mod` modulus) startCode

seqIndex :: Int -> Int -> Int
seqIndex row col = diagStart + col - 1
  where
    n = row + col - 1 -- diagonal number
    diagStart = (n * (n - 1)) `div` 2 -- total codes before this diagonal
