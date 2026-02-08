{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Monad (guard)
import Data.Function (on)
import Data.List (find)
import Data.List.Extra (minimumOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Sequence as Q
import Data.Set ((\\))
import qualified Data.Set as S

type P = (Int, Int)

data Team = E | G deriving (Eq)

data Unit = Unit {uid :: Int, team :: Team, hp :: Int, ap :: Int}

data World = W {walls :: S.Set P, units :: M.Map P Unit}

neigh :: P -> [P]
neigh (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

enemies :: Unit -> World -> [P]
enemies u W {..} = [p | (p, v) <- M.toList units, isEnemy u v]

isEnemy :: Unit -> Unit -> Bool
isEnemy = on (/=) team

-- $setup
-- >>> input = "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######"
-- >>> example = parse (lines input)

main :: IO ()
main = do
  input <- parse . lines <$> readFile "input/2018/15.txt"
  print $ solve input
  print $ partTwo input

parse :: [String] -> World
parse ls = W (S.fromList ws) (M.fromList us)
  where
    coords = [((y, x), c) | (y, row) <- zip [0 ..] ls, (x, c) <- zip [0 ..] row]
    ws = [p | (p, '#') <- coords]
    usOrdered = [(p, c) | (p, c) <- coords, c == 'E' || c == 'G']
    us = [(p, Unit i (if c == 'E' then E else G) 200 3) | (i, (p, c)) <- zip [0 ..] usOrdered]

-- >>> solve example
-- >>> partTwo example
-- 27730
-- 4988
solve, partTwo :: World -> Int
solve w = let (r, w') = simulate w in r * sum (hp <$> units w')
partTwo w =
  head
    [ solve w'
    | ap <- [4 ..],
      let w' = setElfAp ap,
      countElves w' == countElves w,
      countElves (snd (simulate w')) == countElves w
    ]
  where
    countElves = M.size . M.filter ((== E) . team) . units
    setElfAp ap = w {units = (\u -> if team u == E then u {ap} else u) <$> units w}

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
  Just u -> case enemies u w of
    [] -> (w, True)
    foes -> (attack step w {units = units'}, False)
      where
        adjFoe = any (isEnemy u) $ mapMaybe (`M.lookup` units) (neigh pos)
        inRange = S.fromList (concatMap neigh foes) \\ walls \\ M.keysSet units
        step = fromMaybe pos $ firstStep w pos inRange <* guard (not adjFoe)
        units' = if step == pos then units else M.insert step u (M.delete pos units) -- move

attack :: P -> World -> World
attack p w@(W {..}) = case M.lookup p units of
  Nothing -> w
  Just u ->
    case [(q, v) | q <- neigh p, Just v <- [M.lookup q units], isEnemy u v] of
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
    free p = S.notMember p walls && (p == start || M.notMember p units)

    bfs q vis
      | null q = Nothing
      | not (null hits) = Just (snd $ minimum hits)
      | otherwise = uncurry bfs $ foldl expand (Q.empty, vis) q
      where
        hits = Q.filter (\(p, _) -> S.member p targets) q

    expand (acc, v) (p, f)
      | S.member p v = (acc, v)
      | otherwise = (acc <> next, S.insert p v)
      where
        next = Q.fromList [(n, f) | n <- neigh p, free n, S.notMember n v]
