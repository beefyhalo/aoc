import Data.Maybe (fromJust)
import Math.NumberTheory.Moduli (SomeMod, modulo)
import Math.NumberTheory.Moduli.Chinese (chineseSomeMod)

newtype CRT = C {unCRT :: SomeMod} deriving (Show)

instance Semigroup CRT where C x <> C y = C $ fromJust $ chineseSomeMod x y

instance Monoid CRT where mempty = C (0 `modulo` 1)

-- $setup
-- >>> input = "Disc #1 has 5 positions; at time=0, it is at position 4.\nDisc #2 has 2 positions; at time=0, it is at position 1."
-- >>> example = map parse (lines input)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input/2016/15.txt"
  print $ solve input
  print $ partTwo input

parse :: String -> CRT
parse s = C $ (toInteger n - (p + i)) `modulo` n
  where
    ws = words s
    i = read (drop 1 (ws !! 1))
    n = read (ws !! 3)
    p = read (init (ws !! 11))

-- >>> solve example
-- >>> partTwo example
-- 5
-- 85
solve, partTwo :: [CRT] -> SomeMod
solve = unCRT . mconcat
partTwo d = solve (extra : d)
  where
    extra = C $ 11 - (toInteger $ length d + 1) `modulo` 11
