{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Applicative (Alternative)
import Control.Lens
import Control.Monad (guard)
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.Writer.Strict (WriterT, execWriterT, lift, tell)
import Data.Char (isDigit)
import Data.Foldable (for_)
import qualified Data.Heap as H
import Data.Monoid (Sum (..))
import qualified Data.Set as S

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Eq, Ord, Enum, Bounded, Show)

data Player = Player {_hp, _mana, _armor :: Int} deriving (Eq, Show, Ord)

data Boss = Boss {_bossHP, _bossDamage :: Int} deriving (Eq, Show, Ord)

data GameState = GameState
  { _player :: Player,
    _boss :: Boss,
    _effects :: [(Spell, Int)],
    _hardMode :: Bool
  }
  deriving (Eq, Show, Ord)

makeLenses ''Player
makeLenses ''Boss
makeLenses ''GameState

-- Spell data
spellCost :: Spell -> Int
spellCost = \case
  MagicMissile -> 53
  Drain -> 73
  Shield -> 113
  Poison -> 173
  Recharge -> 229

spellDuration :: Spell -> Int
spellDuration = \case
  Shield -> 6
  Poison -> 6
  Recharge -> 5
  _ -> 0

type Turn = WriterT (Sum Int) (StateT GameState [])

-- $setup
-- >>> example = GameState (Player 50 500 0) (Boss 51 9) [] False

main :: IO ()
main = do
  input <- parse <$> readFile "input/2015/22.txt"
  print $ solve input
  print $ solve input {_hardMode = True}

parse :: String -> GameState
parse s = GameState (Player 50 500 0) (Boss bHp bDmg) [] False
  where
    [bHp, bDmg] = map read $ filter (all isDigit) (words s)

-- >>> solve example
-- 900
solve :: GameState -> Int
solve start = go (H.singleton (0, start)) S.empty
  where
    go :: H.MinHeap (Int, GameState) -> S.Set GameState -> Int
    go (H.view -> Just ((ms, st), rest)) seen
      | st ^. boss . bossHP <= 0 = ms
      | st ^. player . hp <= 0 = go rest seen
      | st' `S.member` seen = go rest seen
      | otherwise = go heap' seen'
      where
        newStates = runStateT (execWriterT step) st
        st' = set (player . mana) 0 st
        heap' = foldr (\(Sum cost, s) -> H.insert (ms + cost, s)) rest newStates
        seen' = S.insert st' seen
    go _ _ = error "No solution"

step :: Turn ()
step = do
  whenM (use hardMode) $ player . hp -= 1
  guardM $ uses (player . hp) (> 0)
  applyEffects
  unlessM (uses (boss . bossHP) (<= 0)) $ do
    castSpell
    applyEffects
    unlessM (uses (boss . bossHP) (<= 0)) bossAttack

applyEffects :: Turn ()
applyEffects = do
  player . armor .= 0
  active <- uses effects (map fst)
  for_ active $ \case
    Shield -> player . armor .= 7
    Poison -> boss . bossHP -= 3
    Recharge -> player . mana += 101
    _ -> pure ()
  effects %= filter ((> 0) . snd) . map (_2 -~ 1)

castSpell :: Turn ()
castSpell = do
  s <- lift $ lift [minBound .. maxBound]
  let cost = spellCost s
  guardM $ uses effects (not . any ((== s) . fst))
  guardM $ uses (player . mana) (>= cost)
  tell (Sum cost)
  player . mana -= cost

  case s of
    MagicMissile -> boss . bossHP -= 4
    Drain -> boss . bossHP -= 2 >> player . hp += 2
    _ -> effects <|= (s, spellDuration s)

bossAttack :: Turn ()
bossAttack = do
  dmg <- use (boss . bossDamage)
  arm <- use (player . armor)
  player . hp -= dmg - arm

guardM :: (Monad m, Alternative m) => m Bool -> m ()
guardM = (guard =<<)
