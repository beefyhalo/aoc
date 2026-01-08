import qualified Data.Map.Strict as M
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  input <- V.fromList . map read . words <$> readFile "input/2017/06.txt"
  print $ solve input

-- >>> solve (V.fromList [0,2,7,0])
-- (5,4)
solve :: Vector Int -> (Int, Int)
solve = go M.empty 0
  where
    go seen count current
      | current `M.member` seen = (count, count - (seen M.! current))
      | otherwise = go (M.insert current count seen) (count + 1) (step current)

-- O(val)
step :: Vector Int -> Vector Int
step banks = V.accum (+) (banks // [(maxI, 0)]) updates
  where
    n = V.length banks
    maxI = V.maxIndex banks
    val = banks ! maxI
    updates = [((maxI + i) `mod` n, 1) | i <- [1 .. val]]

-- O(n)
-- step :: Vector Int -> Vector Int
-- step banks = V.generate n update
--   where
--     n = V.length banks
--     maxI = V.maxIndex banks
--     val = banks ! maxI
--     (q, r) = val `divMod` n
--     update i = (if i == maxI then 0 else banks ! i) + q + extra
--       where
--         extra = if (i - maxI + n - 1) `mod` n + 1 <= r then 1 else 0
