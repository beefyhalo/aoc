{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.List (elemIndex, unfoldr, zipWith4)

-- $setup
-- >>> input = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............."
-- >>> example = lines input

main :: IO ()
main = do
  input <- lines <$> readFile "input/2025/07.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve example
-- 21
solve, partTwo :: [String] -> Int
solve (firstRow : restRows) = sum $ unfoldr step initial
  where
    -- Initial state: (current beam row, remaining rows)
    initial :: (String, [String])
    initial = ([bool c '|' (c == 'S') | c <- firstRow], restRows)

    -- Propagate one row
    step (_, []) = Nothing
    step (prevRow, row : rows) = Just . second (,rows) $ go prevRow row (0, [])

    go :: String -> String -> (Int, String) -> (Int, String)
    go (_ : '|' : _ : as) (_ : '^' : xs) (n, acc) = go ('|' : as) xs (n + 1, ".|" ++ acc)
    go (a : as) (_ : xs) (n, acc) = go as xs (n, a : acc)
    go _ _ r = r

-- >>> partTwo example
-- 40
partTwo (firstRow : restRows) = final !! startCol
  where
    Just startCol = elemIndex 'S' firstRow
    final = foldr go (repeat 1) restRows

    go row cur = zipWith4 update row left cur right
      where
        left = 1 : cur
        right = tail cur ++ [1]

        update '^' x _ z = x + z
        update _ _ y _ = y
