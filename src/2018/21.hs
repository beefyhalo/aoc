{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Bits ((.&.), (.|.))
import Data.IntSet qualified as S
import Data.Tuple.Extra (snd3)

main :: IO ()
main = do
  input <- parse <$> readFile "input/2018/21.txt"
  print $ solve input

parse :: String -> Int
parse ls = maximum [read n | line <- lines ls, ["seti", n, _, _] <- [words line]]

-- >>> solve 123
-- (5692748,4081319)
solve :: Int -> (Int, Int)
solve n = (head stream, lastUnique stream)
  where
    _ : stream = iterate (next n) 0

-- mimic this assembly loop
-- loop:
--   acc = mix(acc, r3 & 255)
--   if r3 < 256: break
--   r3 = r3 / 256
--   goto loop
next :: Int -> Int -> Int
next seed acc =
  snd $
    until
      ((== 0) . fst)
      (\(r3, r4) -> (r3 `div` 256, (((r4 + (r3 .&. 255)) .&. 16777215) * 65899) .&. 16777215))
      (acc .|. 65536, seed)

lastUnique :: [Int] -> Int
lastUnique xs =
  snd3 $
    until
      (\(seen, _, y : _) -> S.member y seen)
      (\(seen, _, y : ys) -> (S.insert y seen, y, ys))
      (S.empty, 0, xs)
