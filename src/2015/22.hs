{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Control.Applicative (Alternative)
import Control.Lens
import Control.Monad (guard)
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.State.Strict (StateT, execStateT, lift)
import Data.Char (isDigit)
import Data.Foldable (for_)
import qualified Data.Heap as H
import qualified Data.Set as S

-- Types
data Spell = MagicMissile | Drain | Shield | Poison | Recharge
  deriving (Eq, Ord, Enum, Bounded, Show)

data Player = Player {_hp, _mana, _armor :: Int} deriving (Eq, Show, Ord)

data Boss = Boss {_bossHP, _bossDamage :: Int} deriving (Eq, Show, Ord)

data GameState = GameState
  { _player :: Player,
    _boss :: Boss,
    _effects :: [(Spell, Int)],
    _manaSpent :: Int,
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

type Turn = StateT GameState []

-- $setup
-- >>> example = GameState (Player 50 500 0) (Boss 51 9) [] 0 False

main :: IO ()
main = do
  input <- parse <$> readFile "input/2015/22.txt"
  print $ solve input
  print $ solve input {_hardMode = True}

parse :: String -> GameState
parse s = GameState (Player 50 500 0) (Boss (read bHp) (read bDmg)) [] 0 False
  where
    [bHp, bDmg] = filter (all isDigit) (words s)

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
        newStates = execStateT step st
        st' = st & manaSpent .~ 0 & player . mana .~ 0
        heap' = foldr (\s h -> H.insert (_manaSpent s, s) h) rest newStates
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
  -- reset transient stats
  player . armor .= 0

  -- apply spell effects based on spell type
  active <- uses effects (map fst)
  for_ active $ \case
    Shield -> player . armor .= 7
    Poison -> boss . bossHP -= 3
    Recharge -> player . mana += 101
    _ -> pure ()

  -- Decrement timers and remove expired
  effects %= filter ((> 0) . snd) . map (_2 -~ 1)

castSpell :: Turn ()
castSpell = do
  s <- lift [minBound .. maxBound]
  let cost = spellCost s

  -- guard mana and spell not already active
  guardM $ uses (player . mana) (>= cost)
  guardM $ uses effects (not . any ((== s) . fst))

  -- spend mana
  manaSpent += cost
  player . mana -= cost

  case s of
    MagicMissile -> boss . bossHP -= 4
    Drain -> boss . bossHP -= 2 >> player . hp += 2
    _ -> effects <>:= [(s, spellDuration s)]

bossAttack :: Turn ()
bossAttack = do
  dmg <- use (boss . bossDamage)
  arm <- use (player . armor)
  player . hp -= max 1 (dmg - arm)

guardM :: (Monad m, Alternative m) => m Bool -> m ()
guardM = (guard =<<)
