{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (mapAccumL)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Tuple (swap)

type Point = (Int, Int)

type Dir = (Int, Int)

data Cart = Cart {dir :: Dir, turns :: Int} deriving (Show)

move :: M.Map Point Char -> Point -> Cart -> (Point, Cart)
move track (r, c) (Cart (dr, dc) t) = (nextP, Cart nextD nextT)
  where
    nextP = (r + dr, c + dc)
    (nextD, nextT) = case track M.! nextP of
      '/' -> ((-dc, -dr), t)
      '\\' -> ((dc, dr), t)
      '+' -> ([(-dc, dr), (dr, dc), (dc, -dr)] !! (t `mod` 3), t + 1)
      _ -> ((dr, dc), t)

-- $setup
-- >>> input = unlines ["/>-<\\  ","|   |  ","| /<+-\\","| | | v","\\>+</ |","  |   ^","  \\<->/"]
-- >>> example = parse (lines input)

main :: IO ()
main = do
  input <- parse . lines <$> readFile "input/2018/13.txt"
  print $ solve input

parse :: [String] -> (M.Map Point Char, M.Map Point Cart)
parse input = (track, M.fromList carts)
  where
    raw = [((r, c), ch) | (r, row) <- zip [0 ..] input, (c, ch) <- zip [0 ..] row]
    track = M.fromList [(p, clean ch) | (p, ch) <- raw, ch /= ' ']
    carts = [(p, Cart (d ch) 0) | (p, ch) <- raw, ch `elem` "^v<>"]

    clean c = case c of '^' -> '|'; 'v' -> '|'; '<' -> '-'; '>' -> '-'; x -> x
    d c = case c of '^' -> (-1, 0); 'v' -> (1, 0); '<' -> (0, -1); '>' -> (0, 1)

-- >>> solve example
-- ((2,0),(6,4))
solve :: (M.Map Point Char, M.Map Point Cart) -> (Point, Point)
solve (track, initialCarts) = (swap crashPoint, swap lastCartPoint)
  where
    states = iterate (tick track . fst) (initialCarts, [])
    crashPoint = head [p | (_, cs@(_ : _)) <- states, p <- cs]
    lastCartPoint = head [p | (cs, _) <- states, M.size cs == 1, let (p, _) = head (M.toAscList cs)]

tick :: M.Map Point Char -> M.Map Point Cart -> (M.Map Point Cart, [Point])
tick track carts = (finalMap, catMaybes crashes)
  where
    ps = S.toList $ M.keysSet carts
    (finalMap, crashes) = mapAccumL step carts ps

    step m p = case M.updateLookupWithKey (\_ _ -> Nothing) p m of
      (Nothing, _) -> (m, Nothing)
      (Just ct, m')
        | p' `M.member` m' -> (M.delete p' m', Just p')
        | otherwise -> (M.insert p' ct' m', Nothing)
        where
          (p', ct') = move track p ct
