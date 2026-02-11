{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Comonad (extend, extract)
import Control.Comonad.Store (experiment)
import Control.Lens (view)
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as M
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat, natVal, type (<=))
import qualified GHC.TypeNats as GHC
import SizedGrid hiding (Grid)

data Cell = Open | Trees | Lumber deriving (Eq, Ord, Enum)

type Grid n = FocusedGrid '[HardWrap n, HardWrap n] Cell

gridHash :: Grid n -> Int
gridHash = foldl' (\h c -> h * 3 + fromEnum c) 0

-- $setup
-- >>> input = ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."
-- >>> example = parse @10 input

main :: IO ()
main = do
  input <- parse @50 <$> readFile "input/2018/18.txt"
  print $ solve input

parse :: forall n. (KnownNat n, 1 <= n, KnownNat (n GHC.* n)) => String -> Grid n
parse = view asFocusedGrid . fromJust . gridFromList . map (map parseCell) . lines
  where
    parseCell = \case '.' -> Open; '|' -> Trees; '#' -> Lumber

-- >>> solve example
-- (1147,0)
solve :: (KnownNat n, 1 <= n) => Grid n -> (Int, Int)
solve fg = (resourceValue part1, resourceValue part2)
  where
    next = extend step
    part1 = iterate next fg !! 10
    part2 = iterate next loopStart !! remSteps

    -- Detect cycle
    (seen, endIdx, loopStart) =
      until
        (\(m, _, g) -> M.member (gridHash g) m)
        (\(m, i, g) -> (M.insert (gridHash g) i m, i + 1, next g))
        (M.empty, 0, fg)

    startIdx = seen M.! gridHash loopStart
    remSteps = (1000000000 - startIdx) `mod` (endIdx - startIdx)

step :: (KnownNat n, 1 <= n) => Grid n -> Cell
step fg = case extract fg of
  Open -> if count Trees >= 3 then Trees else Open
  Trees -> if count Lumber >= 3 then Lumber else Trees
  Lumber -> if count Lumber >= 1 && count Trees >= 1 then Lumber else Open
  where
    count c = length $ filter (== c) neighs
    -- neighs = experiment (filter (/= pos fg) . nubOrd . moorePoints 1) fg
    neighs = experiment neighborsOf fg

resourceValue :: Grid n -> Int
resourceValue fg = trees * lumber
  where
    cs = toList fg
    trees = length $ filter (== Trees) cs
    lumber = length $ filter (== Lumber) cs

-- Manual neighbourhood positions
neighborsOf :: forall n. (KnownNat n, 1 <= n) => Coord '[HardWrap n, HardWrap n] -> [Coord '[HardWrap n, HardWrap n]]
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
