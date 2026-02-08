{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (find)
import Data.List.Extra (minimumOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Q
import qualified Data.Set as S

type P = (Int, Int)

data Team = E | G deriving (Eq)

data Unit = Unit {uid :: Int, team :: Team, hp :: Int, ap :: Int}

data World = W {walls :: S.Set P, units :: M.Map P Unit}

neigh :: P -> [P]
neigh (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

enemies :: Team -> World -> [P]
enemies t W {..} = [p | (p, u) <- M.toList units, team u /= t]

-- $setup
-- >>> input = "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######"
-- >>> example = parse (lines input)

main :: IO ()
main = do
  input <- parse . lines <$> readFile "input/2018/15.txt"
  print $ solve input

parse :: [String] -> World
parse ls = W (S.fromList ws) (M.fromList us)
  where
    coords = [((y, x), c) | (y, row) <- zip [0 ..] ls, (x, c) <- zip [0 ..] row]
    ws = [p | (p, '#') <- coords]
    usOrdered = [(p, c) | (p, c) <- coords, c == 'E' || c == 'G']
    us = [(p, Unit i (if c == 'E' then E else G) 200 3) | (i, (p, c)) <- zip [0 ..] usOrdered]

-- >>> solve example
-- (47,590,27730)
solve :: World -> (Int, Int, Int)
solve input = (r, hpLeft, outcome)
  where
    (r, w) = simulate input
    hpLeft = sum $ hp <$> units w
    outcome = r * hpLeft

simulate :: World -> (Int, World)
simulate w0 = (r - 1, w')
  where
    rounds = iterate (runRound . fst) (w0, False)
    Just (r, (w', _)) = find (snd . snd) $ zip [0 ..] rounds

    runRound :: World -> (World, Bool)
    runRound w = M.foldlWithKey' step (w, False) (units w)
      where
        step (world, True) _ _ = (world, True) -- combat already ended
        step (world, False) pos u
          | Just u' <- M.lookup pos (units world), uid u == uid u' = turn pos world
          | otherwise = (world, False)

turn :: P -> World -> (World, Bool)
turn pos w@(W {..}) = case M.lookup pos units of
  Nothing -> (w, False)
  Just u -> case enemies (team u) w of
    [] -> (w, True)
    foes -> (attack step w {units = units'}, False)
      where
        adjFoe = any (\p -> maybe False ((/= team u) . team) (M.lookup p units)) (neigh pos)
        inRange = S.fromList [q | fp <- foes, q <- neigh fp, S.notMember q walls, M.notMember q units]
        step = if adjFoe then pos else fromMaybe pos (firstStep w pos inRange)
        units' = if step == pos then units else M.insert step u (M.delete pos units) -- move

attack :: P -> World -> World
attack p w@(W {..}) = case M.lookup p units of
  Nothing -> w
  Just u ->
    case [(q, v) | q <- neigh p, Just v <- [M.lookup q units], team v /= team u] of
      [] -> w
      foes -> w {units = M.alter upd victimPos units}
        where
          (victimPos, _) = minimumOn (\(q, v) -> (hp v, q)) foes
          upd (Just v) | hp v > ap u = Just v {hp = hp v - ap u}
          upd _ = Nothing

firstStep :: World -> P -> S.Set P -> Maybe P
firstStep W {..} start targets
  | S.null targets = Nothing
  | otherwise = bfs (Q.fromList [(n, n) | n <- neigh start, free n]) (S.singleton start)
  where
    free p = not (S.member p walls) && (p == start || not (M.member p units))

    bfs q vis
      | Q.null q = Nothing
      | not (null hits) = Just (snd $ minimum hits)
      | otherwise = uncurry bfs $ foldl expand (Q.empty, vis) q
      where
        hits = Q.filter (\(p, _) -> S.member p targets) q

    expand (acc, v) (p, f)
      | S.member p v = (acc, v)
      | otherwise = (acc <> next, S.insert p v)
      where
        next = Q.fromList [(n, f) | n <- neigh p, free n, not (S.member n v)]
