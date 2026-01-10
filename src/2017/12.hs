{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List.Split (splitOn)

type Graph = IM.IntMap [Int]

-- $setup
-- >>> input = "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"
-- >>> example = IM.fromList (map parse (lines input))

main :: IO ()
main = do
  g <- IM.fromList . map parse . lines <$> readFile "input/2017/12.txt"
  print $ solve g
  print $ partTwo g

parse :: String -> (Int, [Int])
parse s = (read a, read <$> splitOn ", " b)
  where
    [a, b] = splitOn " <-> " s

-- >>> solve example
-- >>> partTwo example
-- 6
-- 2
solve, partTwo :: Graph -> Int
solve g = IS.size $ dfs g 0
partTwo g = fst $ IS.foldl' go (0, IS.empty) (IM.keysSet g)
  where
    go (n, seen) x
      | IS.member x seen = (n, seen)
      | otherwise = (n + 1, IS.union seen (dfs g x))

dfs :: Graph -> Int -> IS.IntSet
dfs g = go IS.empty
  where
    go seen node
      | IS.member node seen = seen
      | otherwise = foldl' go (IS.insert node seen) (IM.findWithDefault [] node g)
