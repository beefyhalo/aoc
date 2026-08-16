main :: IO ()
main = do
  input <- read <$> readFile "input/2016/19.txt"
  print $ solve input
  print $ partTwo input

-- >>> solve 5
-- >>> partTwo 5
-- 3
-- 2
solve, partTwo :: Int -> Int
solve n = 2 * (n - 2 ^ floor @Double @Int (logBase 2 $ fromIntegral n)) + 1
partTwo n = n - 3 ^ floor @Double @Int (logBase 3 $ fromIntegral n)
