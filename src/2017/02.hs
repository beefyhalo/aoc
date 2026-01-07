{-# OPTIONS_GHC -Wno-x-partial #-}

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> readFile "input/2017/02.txt"
  print $ solve input
  print $ partTwo input

solve, partTwo :: [[Int]] -> Int
solve xss = sum [maximum xs - minimum xs | xs <- xss]
partTwo xss = sum [head [d | x <- xs, y <- xs, x /= y, (d, 0) <- [x `divMod` y]] | xs <- xss]
