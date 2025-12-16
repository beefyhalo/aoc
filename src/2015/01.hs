module Main (main) where

import Data.List (findIndex, foldl')
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- readFile "input/2015/01.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve "()()"
-- 0
solve, partTwo :: String -> Int
solve = foldl' (\acc c -> acc + if c == '(' then 1 else -1) 0
-- >>> partTwo "()())"
-- 5
partTwo = fromJust . findIndex (< 0) . scanl (\acc c -> acc + if c == '(' then 1 else -1) (0 :: Int)
