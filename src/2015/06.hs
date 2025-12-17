{-# LANGUAGE OverloadedStrings #-}
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

type Pos = (Int, Int)

size, rowWords :: Int
size = 1000
rowWords = (size + 63) `div` 64

index :: Int -> Int -> (Int, Int)
index x y = (y * rowWords + x `div` 64, x `mod` 64)

main :: IO ()
main = do
  input <- lines <$> readFile "input/2015/06.txt"
  print $ solve input

-- >>> solve ["turn on 0,0 through 999,999", "turn off 499,499 through 500,500"]
-- 999996
solve :: [String] -> Int
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
    go f s =
      let [p1, p2] = map parse (splitOn " through " s)
       in opBitRange g f p1 p2

opBitRange :: Grid s -> (Word64 -> Int -> Word64) -> Pos -> Pos -> ST s ()
opBitRange g f (x1, y1) (x2, y2) =
  for_ [y1 .. y2] $ \y ->
    for_ [x1 .. x2] $ \x -> do
      let (wi, bi) = index x y
      MV.modify g (`f` bi) wi
