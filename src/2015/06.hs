{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad.ST (ST, runST)
import Data.Bits (clearBit, complementBit, popCount, setBit)
import Data.Foldable (for_)
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word64)

type Grid s = MV.MVector s Word64

size, rowWords :: Int
size = 1000
rowWords = (size + 63) `div` 64

index :: Int -> Int -> (Int, Int)
index x y = (y * rowWords + x `div` 64, x `mod` 64)

main :: IO ()
main = do
  input <- lines <$> readFile "input/2015/06.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve ["turn on 0,0 through 999,999", "turn off 499,499 through 500,500"]
-- 999996
solve, partTwo :: [String] -> Int
solve ops = runST $ do
  g <- MV.replicate (size * rowWords) 0
  for_ ops (apply g)
  frozen <- V.freeze g
  pure $ V.sum (V.map popCount frozen)

apply :: Grid s -> String -> ST s ()
apply g line
  | Just r <- stripPrefix "turn on " line = go setBit r
  | Just r <- stripPrefix "turn off " line = go clearBit r
  | Just r <- stripPrefix "toggle " line = go complementBit r
  where
    parse s = let (x, _ : y) = break (== ',') s in (read x, read y)
    go f s = do
      let [(x1, y1), (x2, y2)] = map parse (splitOn " through " s)
      for_ [y1 .. y2] $ \y ->
        for_ [x1 .. x2] $ \x -> do
          let (wi, bi) = index x y
          MV.modify g (`f` bi) wi

type Grid2 s = MV.MVector s Int

-- >>> partTwo ["turn on 0,0 through 999,999", "turn off 499,499 through 500,500", "toggle 0,0 through 999,999"]
-- 2999996
partTwo ops = runST $ do
  g <- MV.replicate (size * size) 0
  for_ ops (apply2 g)
  V.sum <$> V.freeze g

apply2 :: Grid2 s -> String -> ST s ()
apply2 g line
  | Just r <- stripPrefix "turn on " line = go (+ 1) r
  | Just r <- stripPrefix "turn off " line = go (\x -> max 0 (x - 1)) r
  | Just r <- stripPrefix "toggle " line = go (+ 2) r
  | otherwise = error "invalid input"
  where
    parse s = let (x, _ : y) = break (== ',') s in (read x, read y)
    go f s = do
      let [(x1, y1), (x2, y2)] = map parse (splitOn " through " s)
      for_ [y1 .. y2] $ \y ->
        for_ [x1 .. x2] $ \x ->
          MV.modify g f (y * size + x)
