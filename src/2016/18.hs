-- $setup
-- >>> input = ".^^.^.^^^^"
-- >>> example = parse input

main :: IO ()
main = do
  input <- parse <$> readFile "input/2016/18.txt"
  print $ solve 40 input
  print $ solve 400000 input

parse :: String -> [Bool]
parse = map (== '^')

-- >>> solve 10 example
-- 38
solve :: Int -> [Bool] -> Int
solve n = sum . map (length . filter not) . take n . iterate nextRow

nextRow :: [Bool] -> [Bool]
nextRow prev = zipWith (/=) (False : prev) (drop 1 prev ++ [False])
