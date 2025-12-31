{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Monad.ST (ST, runST)
import Data.Bit (Bit (Bit), countBits)
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

width, height :: Int
width = 50
height = 6

data Op = Rect Int Int | RotRow Int Int | RotCol Int Int

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2016/08.txt"
  let screen = solve input
  print (countBits screen)
  putStrLn (render screen)

parse :: String -> Op
parse s = case words s of
  ["rect", dim] -> let [a, b] = splitOn "x" dim in Rect (read a) (read b)
  ["rotate", "row", y, _, n] -> RotRow (read $ drop 2 y) (read n)
  ["rotate", "column", x, _, n] -> RotCol (read $ drop 2 x) (read n)

solve :: [Op] -> V.Vector Bit
solve ops = runST $ do
  g <- MV.replicate (width * height) (Bit False)
  for_ ops (apply g)
  V.freeze g

apply :: MV.MVector s Bit -> Op -> ST s ()
apply g = \case
  Rect a b -> for_ [0 .. b - 1] $ \y -> MV.set (MV.slice (y * width) a g) (Bit True)
  RotRow y n -> rotate width (V.generate width $ \x -> y * width + x) n
  RotCol x n -> rotate height (V.generate height $ \y -> y * width + x) n
  where
    rotate len idxs n = do
      snapshot <- V.mapM (MV.read g) idxs
      let rotated = V.generate len $ \i -> snapshot V.! ((i - n) `mod` len)
      V.zipWithM_ (MV.write g) idxs rotated

render :: V.Vector Bit -> String
render = V.foldMap step . V.indexed
  where
    step (i, Bit b) =
      bool ' ' '#' b : if i `mod` width == width - 1 then "\n" else ""
