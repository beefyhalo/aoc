{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (mapAccumL)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type P = (Int, Int)

type Dir = (Char, Int)

type Wire = M.Map P Int

step :: Int -> P -> Char -> P
step n (x, y) = \case
  'U' -> (x, y + n)
  'D' -> (x, y - n)
  'R' -> (x + n, y)
  'L' -> (x - n, y)

-- $setup
-- >>> input = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
-- >>> example = parse input

main :: IO ()
main = print . solve . parse =<< readFile "input/2019/03.txt"

parse :: String -> ([Dir], [Dir])
parse s = (a, b)
  where
    [a, b] = map (\(d : n) -> (d, read n)) . splitOn "," <$> lines s

-- >>> solve example
-- (159,610)
solve :: ([Dir], [Dir]) -> (Int, Int)
solve (a, b) = (minimum dists, minimum inter)
  where
    inter = M.intersectionWith (+) (wire a) (wire b)
    dists = [abs x + abs y | (x, y) <- M.keys inter]

wire :: [Dir] -> Wire
wire = M.fromList . concat . snd . mapAccumL expand ((0, 0), 0)
  where
    expand (p, s) (d, n) = ((p', s + n), zip pts [s + 1 ..])
      where
        p' = step n p d
        pts = [step i p d | i <- [1 .. n]]
