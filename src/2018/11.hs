{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.AffineSpace ((.+^))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Rep (index, tabulate)
import Data.Grid.Sized
import Data.List.Extra (maximumOn)
import Data.Tuple.Extra (thd3)

type Dims = '[Clamped 300, Clamped 300]

power :: Int -> Coord Dims -> Int
power serial ((fromEnum -> y) :| (fromEnum -> x) :| _) =
  ((rack * y + serial) * rack `div` 100) `mod` 10 - 5
  where
    rack = x + 10

main :: IO ()
main = do
  input <- makeSAT . read <$> readFile "input/2018/11.txt"
  print $ solve 3 input
  print $ partTwo input

makeSAT :: Int -> Grid Dims Int
makeSAT = transposeGrid . rowPrefix . transposeGrid . rowPrefix . tabulate . power
  where
    rowPrefix = runIdentity . mapLowerDim (Identity . scanl1Grid (+))

-- >>> solve 3 (makeSAT 18)
-- ((33,45),29)
solve :: Int -> Grid Dims Int -> ((Int, Int), Int)
solve size sat =
  maximumOn
    snd
    [ ((c, r), squareSum size p sat)
    | p@((fromEnum -> r) :| (fromEnum -> c) :| _) <- allCoord,
      r + size <= 300,
      c + size <= 300
    ]

-- >>> partTwo (makeSAT 18)
-- ((90,269),16,113)
partTwo :: Grid Dims Int -> ((Int, Int), Int, Int)
partTwo sat =
  maximumOn thd3 [(c, size, v) | size <- [1 .. 16], let (c, v) = solve size sat]

squareSum :: Int -> Coord Dims -> Grid Dims Int -> Int
squareSum size p sat =
  sum [s * index sat (p .+^ off) | (off, s) <- corners]
  where
    d = fromIntegral (size - 1)
    corners =
      [ (coordFromTuple (d, d), 1),
        (coordFromTuple (-1, d), -1),
        (coordFromTuple (d, -1), -1),
        (coordFromTuple (-1, -1), 1)
      ]
