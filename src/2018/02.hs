{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (group, sort)

main :: IO ()
main = do
  input <- lines <$> readFile "input/2018/02.txt"
  print $ solve input
  putStrLn $ partTwo input

-- >>> solve ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
-- 12
solve :: [String] -> Int
solve xs = count (has 2) xs * count (has 3) xs
  where
    count p = length . filter p
    has n = any ((== n) . length) . group . sort

-- >>> partTwo ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
-- "fgij"
partTwo :: [String] -> String
partTwo xs =
  head
    [ grp
    | i <- [0 .. length (head xs) - 1],
      let removeAt s = let (a, b) = splitAt i s in a ++ drop 1 b,
      (grp : _ : _) <- group . sort $ map removeAt xs
    ]