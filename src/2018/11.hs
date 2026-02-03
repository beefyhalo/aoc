{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Comonad (extend)
import Control.Comonad.Store (experiment, peek)
import Data.Functor.Rep (tabulate)
import Data.List (maximumBy)
import Data.Ord (comparing)
import SizedGrid

-- $setup
-- >>> let test serial = let g = fuelGrid serial in solve g

-- >>> test 18
-- >>> test 42
-- (Coord [HardWrap {unHardWrap = Ordinal (45/300)}, HardWrap {unHardWrap = Ordinal (33/300)}],29)
-- (Coord [HardWrap {unHardWrap = Ordinal (61/300)}, HardWrap {unHardWrap = Ordinal (21/300)}],30)

main :: IO ()
main = do
  input <- fuelGrid . read <$> readFile "input/2018/11.txt"
  print $ solve input

power :: Int -> Coord '[HardWrap 300, HardWrap 300] -> Int
power serial (r :| c :| _) = pl - 5
  where
    x = fromEnum c + 1
    y = fromEnum r + 1
    rack = x + 10
    pl = ((rack * y + serial) * rack `div` 100) `mod` 10

fuelGrid :: Int -> FocusedGrid '[HardWrap 300, HardWrap 300] Int
fuelGrid serial =
  FocusedGrid
    { focusedGrid = tabulate (power serial),
      focusedGridPosition = zeroCoord
    }

square3Sum :: FocusedGrid '[HardWrap 300, HardWrap 300] Int -> Int
square3Sum = sum . experiment (moorePoints 1)

solve :: FocusedGrid '[HardWrap 300, HardWrap 300] Int -> (Coord '[HardWrap 300, HardWrap 300], Int)
solve fg = maximumBy (comparing snd) candidates
  where
    sums = extend square3Sum fg
    candidates = [(c, peek c sums) | c <- allCoord]
