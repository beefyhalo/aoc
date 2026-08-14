{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Comonad (extend, extract)
import Control.Comonad.Store (experiment, pos)
import Control.Lens (view)
import Data.Foldable (toList)
import Data.Grid.Sized
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, natVal, type (<=))
import qualified GHC.TypeLits as GHC

data Cell = Alive | Dead deriving (Eq, Show)

-- $setup
-- >>> input = ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."
-- >>> Just example = view asFocusedGrid <$> parse @6 input

main :: IO ()
main = do
  Just (view asFocusedGrid -> input) <- parse @100 <$> readFile "input/2015/18.txt"
  print $ solve 100 input
  print $ partTwo 100 input

parse :: (KnownNat n, KnownNat (n GHC.* n)) => String -> Maybe (Grid '[Clamped n, Clamped n] Cell)
parse = gridFromList . map (map (\c -> if c == '#' then Alive else Dead)) . lines

-- >>> solve 2 example
-- >>> partTwo 2 example
-- 8
-- 14
solve, partTwo :: (KnownNat n, 1 <= n) => Int -> FocusedGrid '[Clamped n, Clamped n] Cell -> Int
solve n = length . filter (== Alive) . toList . (!! n) . iterate (extend step)
partTwo n = length . filter (== Alive) . toList . (!! n) . iterate (extend stepCorners)

type Rule n = (KnownNat n, 1 <= n) => FocusedGrid '[Clamped n, Clamped n] Cell -> Cell

step :: Rule n
step fg
  | here == Alive && aliveCount `elem` [2, 3] = Alive
  | here == Dead && aliveCount == 3 = Alive
  | otherwise = Dead
  where
    here = extract fg
    -- neighs = experiment (nubOrd . filter (/= pos fg) . moorePoints 1) fg
    neighs = experiment neighborsOf fg
    aliveCount = length $ filter (== Alive) neighs

stepCorners :: Rule n
stepCorners fg
  | isCorner (pos fg) = Alive
  | otherwise = step fg

-- Manual neighbourhood positions
neighborsOf :: forall n. (KnownNat n, 1 <= n) => Coord '[Clamped n, Clamped n] -> [Coord '[Clamped n, Clamped n]]
neighborsOf (r :| c :| _) =
  [ toEnum nr :| toEnum nc :| EmptyCoord
  | dr <- [-1 .. 1],
    dc <- [-1 .. 1],
    (dr, dc) /= (0, 0),
    let nr = fromEnum r + dr,
    let nc = fromEnum c + dc,
    nr >= 0 && nr < nVal && nc >= 0 && nc < nVal
  ]
  where
    nVal = fromIntegral (natVal (Proxy @n))
