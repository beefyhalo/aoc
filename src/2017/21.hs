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

-- >>> iterations = iterate (over subSquares example) [".#.", "..#", "###"]
-- >>> sum [1 | g <- iterations !! 2, '#' <- g]
-- 12
solve :: (Grid -> Grid) -> (Int, Int)
solve rules = (countOn 5, countOn 18)
  where
    iterations = iterate (over subSquares rules) start
    start = [".#.", "..#", "###"]
    countOn n = sum [1 | g <- iterations !! n, '#' <- g]

subSquares :: Traversal' Grid Grid
subSquares f g = (partitioned n . traverse . traverse) f g
  where
    n = if even (length g) then 2 else 3

partitioned :: Int -> Iso' Grid [[Grid]]
partitioned n = iso split join
  where
    split = map (transpose . map (chunksOf n)) . chunksOf n
    join = concatMap (map concat . transpose)

variants :: Grid -> [Grid]
variants g = [f r | r <- take 4 (iterate rotateCCW g), f <- [id, reverse]]
  where
    rotateCCW = reverse . transpose
