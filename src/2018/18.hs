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
import Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import GHC.TypeNats (KnownNat, type (<=))
import qualified GHC.TypeNats as GHC
import SizedGrid

data Cell = Open | Trees | Lumber deriving (Eq, Ord)

type FocusedForest n = FocusedGrid '[HardWrap n, HardWrap n] Cell

-- $setup
-- >>> input = ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."
-- >>> example = parse @10 input

main :: IO ()
main = do
  input <- parse @50 <$> readFile "input/2018/18.txt"
  print $ solve input

parse :: forall n. (KnownNat n, 1 <= n, KnownNat (n GHC.* n)) => String -> FocusedForest n
parse = view asFocusedGrid . fromJust . gridFromList . map (map parseCell) . lines
  where
    parseCell = \case '.' -> Open; '|' -> Trees; '#' -> Lumber

-- >>> solve example
-- (1147,0)
solve :: (KnownNat n, 1 <= n) => FocusedForest n -> (Int, Int)
solve fg = (resourceValue part1, resourceValue part2)
  where
    next = extend step
    part1 = iterate next fg !! 10
    part2 = iterate next loopStart !! remSteps

    -- Detect cycle
    (seen, endIdx, loopStart) =
      until
        (\(m, _, c) -> toList c `M.member` m)
        (\(m, i, c) -> (M.insert (toList c) i m, i + 1, next c))
        (M.empty, 0, fg)

    startIdx = seen M.! toList loopStart
    remSteps = (1000000000 - startIdx) `mod` (endIdx - startIdx)

step :: (KnownNat n, 1 <= n) => FocusedForest n -> Cell
step fg = case extract fg of
  Open -> if count Trees >= 3 then Trees else Open
  Trees -> if count Lumber >= 3 then Lumber else Trees
  Lumber -> if count Lumber >= 1 && count Trees >= 1 then Lumber else Open
  where
    count c = length $ filter (== c) (neigh fg)
    neigh = experiment (\p -> filter (/= p) $ nubOrd $ moorePoints 1 p)

resourceValue :: FocusedForest n -> Int
resourceValue fg = trees * lumber
  where
    g = toList fg
    trees = length $ filter (== Trees) g
    lumber = length $ filter (== Lumber) g
