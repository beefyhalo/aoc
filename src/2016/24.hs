{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Array
import Data.Bits
import Data.Char (digitToInt, isDigit)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Q
import qualified Data.Set as S

type Coord = (Int, Int)

inf :: Int
inf = 10 ^ (6 :: Int)

-- $setup
-- >>> input = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########"
-- >>> grid = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2016/24.txt"
  print $ solve False input
  print $ solve True input

parse :: String -> Array Coord Char
parse s = listArray ((0, 0), (h - 1, w - 1)) (concat rows)
  where
    rows = lines s
    (h, w) = (length rows, length (head rows))

allDists :: Array Coord Char -> (Int, Array Coord Int)
allDists grid = (n, dists)
  where
    pois = [(digitToInt c, p) | (p, c) <- assocs grid, isDigit c]
    n = maximum (map fst pois)
    dists = array ((0, 0), (n, n)) [((i, j), bfs grid p1 p2) | (i, p1) <- pois, (j, p2) <- pois]

-- Held-Karp TSP
-- >>> solve False grid
-- >>> solve True grid
-- 14
-- 20
solve :: Bool -> Array Coord Char -> Int
solve returnToStart grid = minimum [dp ! (fullMask, i) + endCost i | i <- [0 .. n]]
  where
    (n, dists) = allDists grid
    fullMask = (1 `shiftL` (n + 1)) - 1

    dp :: Array Coord Int
    dp = array ((1, 0), (fullMask, n)) [((m, l), cost m l) | m <- [1 .. fullMask], l <- [0 .. n]]

    cost m l
      | m == bit 0 = 0
      | otherwise =
          let prevMask = clearBit m l
              candidates = [dp ! (prevMask, p) + dists ! (p, l) | p <- [0 .. n], testBit prevMask p]
           in minimum (inf : candidates)

    endCost i = if returnToStart then dists ! (i, 0) else 0

bfs :: Array Coord Char -> Coord -> Coord -> Int
bfs grid start end = go (Q.singleton (start, 0)) (S.singleton start)
  where
    go Empty _ = error "no path found!"
    go ((curr, d) :<| rest) seen
      | curr == end = d
      | otherwise = go rest' seen'
      where
        next = [p | p <- neighbors curr, inRange (bounds grid) p, grid ! p /= '#', S.notMember p seen]
        seen' = foldr S.insert seen next
        rest' = rest <> Q.fromList [(p, d + 1) | p <- next]
        neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
