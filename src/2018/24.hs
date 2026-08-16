{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Function (on)
import Data.Functor (($>))
import Data.IntMap.Strict qualified as IM
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.MonoTraversable (maximumByMay)
import Data.Ord (Down (..), comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Read (readMaybe)

data Team = Immune | Infection deriving (Eq, Show)

data AttackType = Fire | Cold | Slashing | Radiation | Bludgeoning deriving (Eq, Ord, Read)

data Group = Group
  { team :: Team,
    units, hp, attack, initiative :: Int,
    attackType :: AttackType,
    weak, immune :: S.Set AttackType
  }

type Battle = IM.IntMap Group

effectivePower :: Group -> Int
effectivePower g = units g * attack g

damageTo :: Group -> Group -> Int
damageTo a d
  | attackType a `S.member` immune d = 0
  | attackType a `S.member` weak d = 2 * effectivePower a
  | otherwise = effectivePower a

totalUnits :: Battle -> Int
totalUnits = sum . map units . IM.elems

-- $setup
-- >>> input = "Immune System:\n17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\nInfection:\n801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4\n"
-- >>> Right example = parseOnly parser input

main :: IO ()
main = print . solve . either error id . parseOnly parser =<< TIO.readFile "input/2018/24.txt"

parser :: Parser Battle
parser = do
  blue <- "Immune System:\n" *> many1 (pGroup Immune <* endOfLine) <* endOfLine
  red <- "Infection:\n" *> many1 (pGroup Infection <* option () endOfLine)
  pure . IM.fromAscList $ zip [1 ..] (blue ++ red)
  where
    pGroup tm = do
      u <- decimal <* " units each with "
      h <- decimal <* " hit points "
      (w, i) <- pMods
      atk <- "with an attack that does " *> decimal <* " "
      t <- pAtk <* " damage at initiative "
      ini <- decimal
      pure $ Group tm u h atk ini t w i

    pMods = option (S.empty, S.empty) ("(" *> (f <$> pMod `sepBy` "; ") <* ") ")
      where
        pMod = (,) <$> (("weak to " $> True) <|> ("immune to " $> False)) <*> (S.fromList <$> pAtk `sepBy` ", ")
        f ms = (S.unions [s | (True, s) <- ms], S.unions [s | (False, s) <- ms])

    pAtk = fromJust . readMaybe . T.unpack . T.toTitle <$> takeWhile1 (`notElem` (" ,);" :: String))

-- >>> solve example
-- (5216,51)
solve :: Battle -> (Int, Int)
solve input = (totalUnits final, totalUnits boosted)
  where
    final = simulate input
    boosted =
      head
        [b | n <- [0 ..], let b = simulate (boost Immune n input), all ((== Immune) . team) b]

simulate :: Battle -> Battle
simulate = until done step
  where
    done b = on (==) totalUnits b (step b)
    step b = IM.filter ((> 0) . units) $ attackPhase b (targetPhase b)

targetPhase :: Battle -> [(Int, Int)]
targetPhase b = snd $ foldl' select (b, []) attackers
  where
    attackers = sortOn (\(_, g) -> Down (effectivePower g, initiative g)) $ IM.toList b

    select (avail, acc) (aid, atk)
      | Just (did, _) <- best = (IM.delete did avail, (aid, did) : acc)
      | otherwise = (avail, acc)
      where
        enemies = IM.filter ((/= team atk) . team) avail
        tgts = IM.filter ((> 0) . damageTo atk) enemies
        best = maximumByMay (comparing (key . snd)) $ IM.toList tgts

        key def = (damageTo atk def, effectivePower def, initiative def)

attackPhase :: Battle -> [(Int, Int)] -> Battle
attackPhase start ts = foldl' atk start order
  where
    order = sortOn (\(aid, _) -> Down $ initiative (start IM.! aid)) ts

    atk b (aid, did)
      | (Just a, Just d) <- (IM.lookup aid b, IM.lookup did b),
        units a > 0 =
          IM.adjust (\g -> g {units = max 0 (units g - damageTo a d `div` hp d)}) did b
      | otherwise = b

boost :: Team -> Int -> Battle -> Battle
boost tm n = IM.map f
  where
    f g
      | team g == tm = g {attack = attack g + n}
      | otherwise = g