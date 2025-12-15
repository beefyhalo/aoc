module Main (main) where

import Data.Bifunctor (second)
import qualified Data.Map.Lazy as M

-- $setup
-- >>> input = "aaa: you hhh\nyou: bbb ccc\nbbb: ddd eee\nccc: ddd eee fff\nddd: ggg\neee: out\nfff: out\nggg: out\nhhh: ccc fff iii\niii: out"
-- >>> input2 = "svr: aaa bbb\naaa: fft\nfft: ccc\nbbb: tty\ntty: ccc\nccc: ddd eee\nddd: hub\nhub: fff\neee: dac\ndac: fff\nfff: ggg hhh\nggg: out\nhhh: out"
-- >>> example = parse input
-- >>> example2 = parse input2

type Node = String

type Graph = M.Map Node [Node]

main :: IO ()
main = do
  input <- parse <$> readFile "input/2025/11.txt"
  print $ solve input "you" "out"
  print $ partTwo input "svr" "out"

parse :: String -> Graph
parse = M.fromList . map parseLine . lines
  where
    parseLine :: String -> (String, [String])
    parseLine = second (words . tail) . splitAt 3

-- >>> solve example "you" "out"
-- 5
solve, partTwo :: Graph -> Node -> Node -> Int
solve g start end = memo M.! start
  where
    memo = M.insert end 1 (M.mapWithKey go g)
    go n _ = sum [memo M.! c | c <- g M.! n]

-- >>> partTwo example2 "svr" "out"
-- 2
partTwo g start end = memo M.! (start, False, False)
  where
    memo = M.fromList [((n, d, f), go n d f) | n <- end : M.keys g, d <- [False, True], f <- [False, True]]

    go n d f
      | n == end = if d && f then 1 else 0
      | otherwise = sum [memo M.! (c, d', f') | let d' = d || n == "dac", let f' = f || n == "fft", c <- g M.! n]
