{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Char (isDigit)
import Data.List.Split (wordsBy)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Q
import qualified Data.Set as S

data Cell = Clay | Sand | Flowing | Settled deriving (Eq, Show)

type Pos = (Int, Int)

type Grid = M.Map Pos Cell

isSolid :: Pos -> Grid -> Bool
isSolid p g = M.lookup p g `elem` [Just Clay, Just Settled]

-- $setup
-- >>> input = "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2018/17.txt"
  print $ solve input

parse :: String -> Grid
parse = M.fromList . concatMap go . lines
  where
    nums = map read . wordsBy (not . isDigit)
    go l = case head l of
      'x' -> let [x, y1, y2] = nums l in [((x, y), Clay) | y <- [y1 .. y2]]
      'y' -> let [y, x1, x2] = nums l in [((x, y), Clay) | x <- [x1 .. x2]]
      _ -> []

-- >>> solve example
-- (57,29)
solve :: Grid -> (Int, Int)
solve grid = (M.size water, M.size $ M.filter (== Settled) water)
  where
    keys = S.map snd $ M.keysSet grid
    (minY, maxY) = (minimum keys, maximum keys)
    final = flow maxY (500, 0) grid
    water = M.filterWithKey (\(_, y) c -> y >= minY && c `elem` [Flowing, Settled]) final

flow :: Int -> Pos -> Grid -> Grid
flow maxY start g = fst $ until (Q.null . snd) step (g, Q.singleton start)
  where
    step (g', p :<| qs) = (gFilled, qs <> Q.fromList newSources)
      where
        (gFilled, newSources) = flowFrom maxY p g'

flowFrom :: Int -> Pos -> Grid -> (Grid, [Pos])
flowFrom maxY p@(x, y) g
  | y > maxY || isSolid p g = (g, [])
  | isSolid below g' = spread p g'
  | otherwise = flowFrom maxY below g'
  where
    g' = M.insert p Flowing g
    below = (x, y + 1)

spread :: Pos -> Grid -> (Grid, [Pos])
spread (x, y) g
  | bounded && y > 0 = spread (x, y - 1) gFilled
  | bounded = (gFilled, [])
  | otherwise = (gFilled, [(lX, y) | not lWall] ++ [(rX, y) | not rWall])
  where
    (lX, lWall) = findBound x (-1)
    (rX, rWall) = findBound x 1
    bounded = lWall && rWall
    fillType = if bounded then Settled else Flowing
    gFilled = foldr (\nx -> M.insert (nx, y) fillType) g [lX .. rX]

    findBound cur dx
      | M.lookup (cur + dx, y) g == Just Clay = (cur, True)
      | isSolid (cur, y + 1) g = findBound (cur + dx) dx
      | otherwise = (cur, False)
