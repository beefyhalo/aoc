import Data.Set qualified as S

main :: IO ()
main = do
  input <- readFile "input/2015/03.txt"
  print $ solve input
  print $ partTwo input

-- >>> map solve [">", "^>v<", "^v^v^v^v^v"]
-- [2,4,2]
solve, partTwo :: String -> Int
solve = S.size . S.fromList . scanl move (0, 0)

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) = \case
  '^' -> (x, y + 1)
  'v' -> (x, y - 1)
  '>' -> (x + 1, y)
  _ -> (x - 1, y)

-- >>> map partTwo ["^v", "^>v<", "^v^v^v^v^v"]
-- [3,3,11]
partTwo =
  S.size . S.fromList . uncurry (++) . unzip . scanl step ((0, 0), (0, 0)) . zip [0 :: Int ..]
  where
    step (santa, robo) (i, c)
      | even i = (move santa c, robo)
      | otherwise = (santa, move robo c)
