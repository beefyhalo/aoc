{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Control.Monad.State.Strict (MonadState (get), State, evalState, gets, modify)
import Control.Monad.Trans.Free (FreeF (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Fix (refold, refoldM)
import Data.List (delete, permutations, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type City = String

type Distance = Int

-- $setup
-- >>> input = "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141"
-- >>> example = M.fromList $ concatMap parse (lines input)

main :: IO ()
main = do
  input <- M.fromList . concatMap parse . lines <$> readFile "input/2015/09.txt"
  print $ solve input
  print $ partTwo input

  print $ solve2 input
  print $ partTwo2 input

  print $ solve3 input

-- print $ partTwo3 input

parse :: String -> [((City, City), Distance)]
parse s = [((c1, c2), read d), ((c2, c1), read d)]
  where
    [c1, rest] = splitOn " to " s
    [c2, d] = splitOn " = " rest

-- Brute-force solution

-- >>> solve example
-- >>> partTwo example
-- 605
-- 982
solve, partTwo :: M.Map (City, City) Distance -> Int
solve = minimum . routes
partTwo = maximum . routes

routes :: M.Map (City, City) Distance -> [Distance]
routes dists = map routeDistance $ permutations (cities dists)
  where
    routeDistance (x : y : xs) = dists M.! (x, y) + routeDistance (y : xs)
    routeDistance _ = 0

cities :: M.Map (City, City) Distance -> [City]
cities dists = nubOrd [c | (a, b) <- M.keys dists, c <- [a, b]]

-- Alternative solution using structural recursion. Fun!

type TSPF = FreeF [] Distance

type Seed = (Maybe City, [City], Distance)

mkSeed :: M.Map (City, City) Distance -> Seed
mkSeed dist = (Nothing, cities dist, 0)

-- >>> solve2 example
-- 605
solve2, partTwo2 :: M.Map (City, City) Distance -> Distance
solve2 dist = evalState (refoldM algMin coalgMin (mkSeed dist)) maxBound
  where
    algMin :: TSPF Distance -> State Distance Distance
    algMin = \case
      Pure d -> d <$ modify (min d)
      Free dists -> pure $ minimum dists

    coalgMin :: Seed -> State Distance (TSPF Seed)
    coalgMin seed@(_, _, d) = gets $ \case
      best
        | d >= best -> Pure maxBound
        | otherwise -> expand dist seed

expand :: M.Map (City, City) Distance -> Seed -> TSPF Seed
expand dist = \case
  -- Virtual root
  (Nothing, allCities, _) -> Free [(Just c, delete c allCities, 0) | c <- allCities]
  (Just _, [], d) -> Pure d
  (Just cur, cs, d) -> Free $ sortOn (\(_, _, c) -> c) [(Just c, delete c cs, d + dist M.! (cur, c)) | c <- cs]

-- >>> partTwo2 example
-- 982
partTwo2 dist = refold algMax (expand dist) (mkSeed dist)
  where
    algMax = \case
      Pure d -> d
      Free dists -> maximum dists

-- Now Held–Karp DP

-- >>> solve3 example
-- 605
solve3 :: M.Map (City, City) Distance -> Distance
solve3 dists = evalState (refoldM alg coalg (let (s : rest) = cities dists in (s, rest))) M.empty
  where
    alg (_ :< Pure suf) = pure suf
    alg (key@(cur, remSet) :< Free branches) = do
      let res = minimum $ zipWith (\c suf -> dists M.! (cur, c) + suf) (S.toList remSet) branches
      modify (M.insert key res)
      pure res

    coalg (cur, rest) = do
      cache <- get
      let key = (cur, S.fromList rest)
      pure $
        key :< case M.lookup key cache of
          Just suf -> Pure suf
          Nothing | null rest -> Pure 0
          Nothing -> Free [(c, delete c rest) | c <- rest]
