{-# OPTIONS_GHC -Wno-type-defaults #-}

import Control.Monad (foldM, foldM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bit (Bit (Bit), countBits)
import Data.Bits (testBit, xor)
import Data.Char (ord)
import Data.Foldable (for_, traverse_)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- $setup
-- >>> example = parse "flqrgnkx"

main :: IO ()
main = do
  input <- parse <$> readFile "input/2017/14.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> V.Vector Bit
parse key = V.concat [mkRow (key ++ "-" ++ show r) | r <- [0 .. 127]]
  where
    mkRow s = V.fromList [Bit (testBit byte i) | byte <- V.toList $ dense s, i <- [7, 6 .. 0]]

-- >>> solve example
-- >>> partTwo example
-- 8108
-- 1242
solve, partTwo :: Vector Bit -> Int
solve = countBits
partTwo input = runST $ do
  g <- V.thaw input
  foldM (step g) 0 [0 .. V.length input - 1]
  where
    step g acc i = do
      Bit val <- MV.read g i
      if val then acc + 1 <$ flood g (i `divMod` 128) else pure acc

    flood g (r, c)
      | r < 0 || r > 127 || c < 0 || c > 127 = pure ()
      | otherwise = do
          let idx = r * 128 + c
          Bit val <- MV.read g idx
          when val $ do
            MV.write g idx (Bit False) -- Burn the bit
            traverse_ (flood g) [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- From Day 10
dense :: String -> V.Vector Int
dense input = V.generate 16 $ \i -> V.foldl1' xor (V.slice (i * 16) 16 sparse)
  where
    bytes = map ord input ++ [17, 31, 73, 47, 23]
    sparse = runKnot bytes 64 256

runKnot :: [Int] -> Int -> Int -> V.Vector Int
runKnot lengths rounds size =
  V.modify
    (\v -> foldM_ (step v) (0, 0) (concat $ replicate rounds lengths))
    (V.generate size id)
  where
    step v (p, s) l = ((p + l + s) `mod` size, s + 1) <$ rev v p l

rev :: MV.MVector s Int -> Int -> Int -> ST s ()
rev v p l =
  for_ [0 .. l `div` 2 - 1] $ \i ->
    MV.swap v ((p + i) `mod` n) ((p + l - 1 - i) `mod` n)
  where
    n = MV.length v
