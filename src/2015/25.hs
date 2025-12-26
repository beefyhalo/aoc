{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Mod (Mod (..), (^%))

-- The modulus as a type-level natural
startCode, multiplier :: Mod 33554393
startCode = 20151125
multiplier = 252533

-- $setup
-- >>> input = "To continue, please consult the code grid in the manual.  Enter the code at row 2, column 1."
-- >>> (exampleRow, exampleCol) = parse input

main :: IO ()
main = do
  (r, c) <- parse <$> readFile "input/2015/25.txt"
  print $ solve r c

parse :: String -> (Int, Int)
parse s = (r, c)
  where
    [r, c] = read . filter isDigit <$> splitOn ", column " s

-- >>> solve exampleRow exampleCol
-- 31916031
solve :: Int -> Int -> Int
solve r c = fromIntegral . unMod $ startCode * multiplier ^% seqIndex r c

-- solve r c = codes !! seqIndex r c
--   where codes = iterate (\prev -> (prev * multiplier) `mod` modulus) startCode

seqIndex :: Int -> Int -> Int
seqIndex r c = diagStart + c - 1
  where
    n = r + c - 1 -- diagonal number
    diagStart = (n * (n - 1)) `div` 2 -- total codes before this diagonal
