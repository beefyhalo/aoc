import Control.Lens (ix, (&), (+~))

main :: IO ()
main = do
  input <- parse <$> readFile "input/2016/11.txt"
  print $ solve input
  print $ solve (input & ix 0 +~ 4)

parse :: String -> [Int]
parse = map count . lines
  where
    count l = length [() | w : _ <- drop 4 (words l), w `elem` "gm"]

solve :: [Int] -> Int
solve xs = sum [2 * c - 1 | c <- init $ scanl1 (+) xs]
