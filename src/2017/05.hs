{-# LANGUAGE BangPatterns #-}

import Control.Monad.ST (runST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

main :: IO ()
main = do
  input <- V.fromList . map read . lines <$> readFile "input/2017/05.txt"
  print $ solve (+ 1) input
  print $ solve (\o -> o + if o < 3 then 1 else -1) input

-- >>> solve (+1) (V.fromList [0,3,0,1,-3])
-- 5
solve :: (Int -> Int) -> V.Vector Int -> Int
solve update input = runST $ do
  v <- V.thaw input
  let len = V.length input
  let go !p !s
        | p < 0 || p >= len = pure s
        | otherwise = do
            o <- MV.read v p
            MV.write v p (update o)
            go (p + o) (s + 1)
  go 0 0
