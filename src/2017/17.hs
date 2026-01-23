{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

main :: IO ()
main = do
  input <- read <$> readFile "input/2017/17.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve 3
-- >>> partTwo 3
-- 638
-- 1222153
solve, partTwo :: Int -> Int
solve steps = Seq.index buf ((i + 1) `mod` Seq.length buf)
  where
    buf = spinlock steps 2017
    Just i = Seq.elemIndexL 2017 buf
partTwo steps = snd $ foldl' g (0, 0) [1 .. 50_000_000]
  where
    g (!pos, !val) i =
      let pos' = (pos + steps) `mod` i + 1
       in (pos', if pos' == 1 then i else val)

spinlock :: Int -> Int -> Seq Int
spinlock steps n = snd $ foldl' f (0, Seq.singleton 0) [1 .. n]
  where
    f (!pos, !buf) i =
      let pos' = (pos + steps) `mod` Seq.length buf + 1
       in (pos', Seq.insertAt pos' i buf)
