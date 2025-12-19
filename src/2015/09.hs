{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad.State.Strict (State, evalState, gets, modify)
import Data.Containers.ListUtils (nubOrd)
import Data.Fix (refold, refoldM)
import Data.List (delete, permutations, sortOn)
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

  print $ solve2 input
  print $ partTwo2 input

parse :: String -> [((City, City), Distance)]
parse s = [((c1, c2), read d), ((c2, c1), read d)]
  where
    [c1, rest] = splitOn " to " s
    [c2, d] = splitOn " = " rest

-- Brute-force solution

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

-- Alternative solution using structural recursion. Fun!

data TSPF a = Done Distance | Branch [a] deriving (Functor, Foldable, Traversable)

type Seed = (Maybe City, [City], Distance)

mkSeed :: M.Map (City, City) Distance -> Seed
mkSeed dist = (Nothing, nubOrd [c | (a, b) <- M.keys dist, c <- [a, b]], 0)

-- >>> solve2 example
-- 605
solve2, partTwo2 :: M.Map (City, City) Distance -> Distance
solve2 dist = evalState (refoldM algMin coalgMin (mkSeed dist)) maxBound
  where
    algMin :: TSPF Distance -> State Distance Distance
    algMin = \case
      Done d -> d <$ modify (min d)
      Branch dists -> pure $ minimum dists

    coalgMin :: Seed -> State Distance (TSPF Seed)
    coalgMin seed@(_, _, d) = gets $ \case
      best
        | d >= best -> Done maxBound
        | otherwise -> expand dist seed

expand :: M.Map (City, City) Distance -> Seed -> TSPF Seed
expand dist = \case
  -- Virtual root
  (Nothing, allCities, _) -> Branch [(Just c, delete c allCities, 0) | c <- allCities]
  (Just _, [], d) -> Done d
  (Just cur, cs, d) -> Branch $ sortOn (\(_, _, c) -> c) [(Just c, delete c cs, d + dist M.! (cur, c)) | c <- cs]

-- >>> partTwo2 example
-- 982
partTwo2 dist = refold algMax (expand dist) (mkSeed dist)
  where
    algMax = \case
      Done d -> d
      Branch dists -> maximum dists
