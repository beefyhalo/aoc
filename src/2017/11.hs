{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (scanl')
import Data.List.Split (splitOn)
import Linear.V3 (V3 (..))

data Dir = N | NE | SE | S | SW | NW deriving (Show)

main :: IO ()
main = do
  input <- map parse . splitOn "," <$> readFile "input/2017/11.txt"
  print $ solve input

parse :: String -> Dir
parse = \case
  "n" -> N
  "ne" -> NE
  "se" -> SE
  "s" -> S
  "sw" -> SW
  "nw" -> NW

-- >>> map solve [[NE,NE,NE], [NE,NE,SW,SW]]
-- [(3,3),(0,2)]
solve :: [Dir] -> (Int, Int)
solve dirs = (maximum final, maximum maxDist)
  where
    positions = scanl' (\p d -> p + step d) 0 dirs
    final = last positions
    maxDist = map maximum positions

step :: Dir -> V3 Int
step = \case
  N -> V3 0 1 (-1)
  NE -> V3 1 0 (-1)
  SE -> V3 1 (-1) 0
  S -> V3 0 (-1) 1
  SW -> V3 (-1) 0 1
  NW -> V3 (-1) 1 0
