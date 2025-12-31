{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Bit (Bit (Bit), countBits)
import Data.Foldable (for_)
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

width, height :: Int
width = 50
height = 6

data Op = Rect Int Int | RotRow Int Int | RotCol Int Int deriving (Show)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2016/08.txt"
  let screen = solve input
  print (countBits screen)
  putStrLn (render screen)

parse :: String -> Op
parse s = case words s of
  ["rect", dim] -> let [a, b] = splitOn "x" dim in Rect (read a) (read b)
  ["rotate", "row", y, "by", n] -> RotRow (read $ drop 2 y) (read n)
  ["rotate", "column", x, "by", n] -> RotCol (read $ drop 2 x) (read n)

solve :: [Op] -> V.Vector Bit
solve ops = runST $ do
  g <- MV.replicate (width * height) (Bit False)
  for_ ops (apply g)
  V.freeze g

apply :: MV.MVector s Bit -> Op -> ST s ()
apply g = \case
  Rect a b -> for_ [0 .. b - 1] $ \y -> MV.set (MV.slice (y * width) a g) (Bit True)
  RotRow y n -> rotate width (rowIdxs y) n
  RotCol x n -> rotate height (colIdxs x) n
  where
    rowIdxs y = [y * width + x | x <- [0 .. width - 1]]
    colIdxs x = [y * width + x | y <- [0 .. height - 1]]

    rotate len idxs n = do
      vals <- mapM (MV.read g) idxs
      let k = n `mod` len
          rotated = take len $ drop (len - k) (cycle vals)
      zipWithM_ (MV.write g) idxs rotated

render :: V.Vector Bit -> String
render = V.foldMap step . V.indexed
  where
    step (i, Bit b) =
      (if b then '#' else '.') : if i `mod` width == width - 1 then "\n" else ""
