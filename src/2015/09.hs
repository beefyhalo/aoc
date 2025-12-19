{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (permutations)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type City = String

type Distance = Int

-- $setup
-- >>> input = "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141"
-- >>> example = M.fromList $ concatMap parse (lines input)

main :: IO ()
main = do
  input <- M.fromList . concatMap parse . lines <$> readFile "input/2015/09.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> [((City, City), Distance)]
parse s = [((c1, c2), read d), ((c2, c1), read d)]
  where
    [c1, rest] = splitOn " to " s
    [c2, d] = splitOn " = " rest

-- >>> solve example
-- 605
solve, partTwo :: M.Map (City, City) Distance -> Int
solve = minimum . routes
-- >>> partTwo example
-- 982
partTwo = maximum . routes

routes :: M.Map (City, City) Distance -> [Distance]
routes distances = map routeDistance $ permutations cities
  where
    cities = nubOrd [c | (c1, c2) <- M.keys distances, c <- [c1, c2]]
    routeDistance (x : y : xs) = distances M.! (x, y) + routeDistance (y : xs)
    routeDistance _ = 0
