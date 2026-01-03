{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- $setup
-- >>> input = "Disc #1 has 5 positions; at time=0, it is at position 4.\nDisc #2 has 2 positions; at time=0, it is at position 1."
-- >>> example = map parse (lines input)

type Congruence = (Int, Int) -- (modulus, remainder)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2016/15.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> Congruence
parse s = (n, (n - (p + i) `mod` n))
  where
    ws = words s
    i = read (tail (ws !! 1))
    n = read (ws !! 3)
    p = read (init (ws !! 11))

-- >>> solve example
-- >>> partTwo example
-- 5
-- 85
solve, partTwo :: [Congruence] -> Int
solve = snd . foldl' combine (1, 0)
partTwo d = solve (extra : d)
  where
    extra = (11, (11 - (length d + 1) `mod` 11))

-- Combine two congruences:
--   x ≡ a (mod m)
--   x ≡ b (mod n)
combine :: Congruence -> Congruence -> Congruence
combine (m, a) (n, b) = (lcm m n, x `mod` lcm m n)
  where
    x = head [a + k * m | k <- [0 ..], (a + k * m) `mod` n == b]
