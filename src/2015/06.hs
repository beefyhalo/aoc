{-# LANGUAGE LambdaCase #-}
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

size, rowWords :: Int
size = 1000
rowWords = (size + 63) `div` 64

index :: Int -> Int -> (Int, Int)
index x y = (y * rowWords + x `div` 64, x `mod` 64)

type Pos = (Int, Int)

data Op = On Pos Pos | Off Pos Pos | Toggle Pos Pos
  deriving (Show)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2015/06.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> Op
parse s
  | Just r <- stripPrefix "turn on " s = parseAs On r
  | Just r <- stripPrefix "turn off " s = parseAs Off r
  | Just r <- stripPrefix "toggle " s = parseAs Toggle r
  | otherwise = error "invalid input"
  where
    parseAs f r = let [p1, p2] = map parseCoord $ splitOn " through " r in f p1 p2
    parseCoord str = let (x, _ : y) = break (== ',') str in (read x, read y)

-- >>> solve [On (0,0) (999,999), Off (499,499) (500,500)]
-- 999996
solve, partTwo :: [Op] -> Int
solve ops = runST $ do
  g <- MV.replicate (size * rowWords) 0
  for_ ops (applyBit g)
  V.sum . V.map popCount <$> V.freeze g

applyBit :: MV.MVector s Word64 -> Op -> ST s ()
applyBit g = \case
  On p1 p2 -> opBitRange setBit p1 p2
  Off p1 p2 -> opBitRange clearBit p1 p2
  Toggle p1 p2 -> opBitRange complementBit p1 p2
  where
    opBitRange f (x1, y1) (x2, y2) =
      for_ [y1 .. y2] $ \y -> for_ [x1 .. x2] $ \x ->
        let (wi, bi) = index x y in MV.modify g (`f` bi) wi

-- >>> partTwo [On (0,0) (999,999), Off (499,499) (500,500), Toggle (0,0) (999,999)]
-- 2999996
partTwo ops = runST $ do
  g <- MV.replicate (size * size) 0
  for_ ops (applyBright g)
  V.sum <$> V.freeze g

applyBright :: MV.MVector s Int -> Op -> ST s ()
applyBright g = \case
  On p1 p2 -> opIntRange (+ 1) p1 p2
  Off p1 p2 -> opIntRange (\v -> max 0 (v - 1)) p1 p2
  Toggle p1 p2 -> opIntRange (+ 2) p1 p2
  where
    opIntRange f (x1, y1) (x2, y2) =
      for_ [y1 .. y2] $ \y -> for_ [x1 .. x2] $ \x ->
        MV.modify g f (y * size + x)
