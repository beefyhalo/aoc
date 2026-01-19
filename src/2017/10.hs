import Control.Monad (foldM_)
import Control.Monad.ST (ST)
import Data.Bits (xor)
import Data.Char (ord)
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Printf (printf)
import Data.Foldable (for_)

main :: IO ()
main = do
  input <- readFile "input/2017/10.txt"
  print $ solve (read <$> splitOn "," input) 256
  putStrLn $ partTwo input

-- >>> solve [3,4,1,5] 5
-- 12
solve :: [Int] -> Int -> Int
solve lengths size = v V.! 0 * v V.! 1
  where
    v = runKnot lengths 1 size

-- >>> map partTwo ["", "AoC 2017", "1,2,3"]
-- ["a2582a3a0e66e6e86e3812dcb672a272","33efeb34ea91902bb2f59c9920caa6cd","3efbe78a8d82f29979031a4aa0b16a9d"]
partTwo :: String -> String
partTwo input = V.foldMap (printf "%02x") dense
  where
    bytes = map ord input ++ [17, 31, 73, 47, 23]
    sparse = runKnot bytes 64 256
    dense = V.generate 16 $ \i -> V.foldl1' xor (V.slice (i * 16) 16 sparse)

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
