{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Lens
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map.Strict as M

type Grid = [String]

-- $setup
-- >>> input = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2017/21.txt"
  print $ solve input

parse :: String -> (Grid -> Grid)
parse = makeRules . map parseRule . lines
  where
    parseRule (words -> [a, "=>", b]) = (splitOn "/" a, splitOn "/" b)
    makeRules rs = (m M.!)
      where
        m = M.fromList [(k', v) | (k, v) <- rs, k' <- variants k]

-- >>> iterations = iterate (step example) [".#.", "..#", "###"]
-- >>> length $ filter (== '#') $ concat (iterations !! 2)
-- 12
solve :: (Grid -> Grid) -> (Int, Int)
solve rules = (countOn 5, countOn 18)
  where
    iterations = iterate (step rules) start
    start = [".#.", "..#", "###"]
    countOn n = length $ filter (== '#') $ concat (iterations !! n)

step :: (Grid -> Grid) -> Grid -> Grid
step f g = g & partitioned n . traverse . traverse %~ f
  where
    n = if even (length g) then 2 else 3

partitioned :: Int -> Iso' Grid [[Grid]]
partitioned n = iso split join
  where
    split :: Grid -> [[Grid]]
    split = map (transpose . map (chunksOf n)) . chunksOf n

    join :: [[Grid]] -> Grid
    join = concatMap (map concat . transpose)

variants :: Grid -> [Grid]
variants g = [f r | r <- take 4 (iterate rotateCCW g), f <- [id, reverse]]
  where
    rotateCCW = reverse . transpose
