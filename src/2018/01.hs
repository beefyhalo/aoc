{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import qualified Data.IntSet as S
import Data.List.Extra (delete)

main :: IO ()
main = do
  input <- map (read . delete '+') . lines <$> readFile "input/2018/01.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve [1, -2, 3, 1]
-- >>> partTwo [7, 7, -2, -7, -4]
-- 3
-- 14
solve, partTwo :: [Int] -> Int
solve = sum
partTwo = go S.empty . scanl1 (+) . cycle
  where
    go seen (y : ys)
      | S.member y seen = y
      | otherwise = go (S.insert y seen) ys
