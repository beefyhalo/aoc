{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (delete)

type Component = (Int, Int)

-- $setup
-- >>> input = "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10"
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2017/24.txt"
  print $ solve input

parse :: String -> Component
parse s = (read a, read b)
  where
    (a, _ : b) = break (== '/') s

-- >>> solve example
-- (31,19)
solve :: [Component] -> (Int, Int)
solve input = (maximum $ map snd bridges, snd $ maximum bridges)
  where
    bridges = build 0 input

build :: Int -> [Component] -> [(Int, Int)]
build port pool =
  (0, 0)
    : [ (len + 1, str + a + b)
      | (a, b) <- pool,
        next <-
          if
            | a == port -> [b]
            | b == port -> [a]
            | otherwise -> [],
        (len, str) <- build next $ delete (a, b) pool
      ]
