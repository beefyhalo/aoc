{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Arrow ((***))
import Data.Bits (xor, (.&.))
import Data.Mod (Mod (unMod))

type M = Mod 2147483647

main :: IO ()
main = do
  (inputA, inputB) <- parse . lines <$> readFile "input/2017/15.txt"
  print $ solve 40_000_000 inputA inputB
  print $ partTwo 5_000_000 inputA inputB

parse :: [String] -> (M, M)
parse [line1, line2] = (read $ words line1 !! 4, read $ words line2 !! 4)

-- >>> solve 40_000_000 65 8921
-- >>> partTwo 5_000_000 65 8921
-- 588
-- 0
solve, partTwo :: Int -> M -> M -> Int
solve limit a0 b0 =
  sum [1 | (a, b) <- take limit $ iterate ((16807 *) *** (48271 *)) (a0, b0), (unMod a `xor` unMod b) .&. 0xFFFF == 0]
partTwo limit a0 b0 =
  sum [1 | (a, b) <- take limit $ iterate ((next 16807 4) *** (next 48271 8)) (a0, b0), (unMod a `xor` unMod b) .&. 0xFFFF == 0]
  where
    next f m x = let !y = f * x in if unMod y `rem` m == 0 then y else next f m y
