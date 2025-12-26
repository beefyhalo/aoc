{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- (over, (&))

import Control.Arrow ((&&&))
import Control.Comonad (extend, extract)
import Control.Comonad.Store (experiment)
import Control.Lens (view)
import Data.AffineSpace ((.+^))
import Data.Attoparsec.ByteString.Char8 (Parser, char, choice, endOfLine, many1, parseOnly, sepBy)
import Data.Bool (bool)
import Data.ByteString qualified as BS (readFile)
import Data.Constraint (withDict, (\\))
import Data.Constraint.Nat (leTrans, plusMonotone2, plusNat)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Product (Product (Pair))
import Data.Maybe (fromJust, mapMaybe)
import GHC.TypeNats (KnownNat, type (+), type (<=))
import GHC.TypeNats qualified as GHC
import SizedGrid (Grid, IsGrid (asFocusedGrid), Periodic, StrengthenCoord (strengthenCoord), WeakenCoord (weakenCoord), gridFromList)
import SizedGrid.Coord (Coord)

data Cell = X | M | A | S deriving (Eq, Show)

type Input n = Grid '[Periodic n, Periodic n] Cell

-- (2532,1941)
main :: IO ()
main = either print (print . (solve &&& partTwo)) . parseOnly (parser @140) =<< BS.readFile "input/2024/04.txt"

-- $setup
-- >>> import Data.Either (fromRight)
-- >>> example = fromRight undefined $ parseOnly (parser @10) "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
-- >>> example
-- Grid {unGrid = [M,M,M,S,X,X,M,A,S,M,M,S,A,M,X,M,S,M,S,A,A,M,X,S,X,M,A,A,M,M,M,S,A,M,A,S,M,S,M,X,X,M,A,S,A,M,X,A,M,M,X,X,A,M,M,X,X,A,M,A,S,M,S,M,S,A,S,X,S,S,S,A,X,A,M,A,S,A,A,A,M,A,M,M,M,X,M,M,M,M,M,X,M,X,A,X,M,A,S,X]}

parser :: (KnownNat n, KnownNat (n GHC.* n)) => Parser (Input n)
parser = fromJust . gridFromList <$> many1 cParser `sepBy` endOfLine
  where
    cParser = choice [X <$ char 'X', M <$ char 'M', A <$ char 'A', S <$ char 'S']

data Three a = Three a a a deriving (Eq, Functor, Show, Foldable, Traversable)

type Context a = Compose [] Three a

-- >>> solve example
-- 18
solve :: forall n. (1 <= n, KnownNat n) => Input n -> Int
solve = sum . fmap occurences . extend (\s -> if extract s == X then experiment applyContext s else mempty) . view asFocusedGrid
  where
    applyContext :: Coord '[Periodic n, Periodic n] -> Context (Coord '[Periodic n, Periodic n])
    applyContext c =
      Compose $
        mapMaybe
          (traverse weakenCoord)
          [ Three i j k
          | dy <- [-1 .. 1],
            dx <- [-1 .. 1],
            let d = (dy, dx),
            d /= (0, 0),
            let i, j, k :: Coord '[Periodic (n + 1), Periodic (n + 1)]
                i = strengthenCoord c .+^ d
                j = i .+^ d
                k = j .+^ d
          ]
          \\ plusNat @n @1
          \\ leTrans @1 @n @(n + 1)
          \\ plusMonotone2 @n @0 @1

    occurences :: Context Cell -> Int
    occurences = length . filter (== Three M A S) . getCompose

data Two a = Two a a deriving (Eq, Show, Functor, Foldable, Traversable)

type ContextTwo a = Compose Maybe (Product Two Two) a

-- >>> partTwo example
-- 9
partTwo :: forall n. (1 <= n, KnownNat n) => Input n -> Int
partTwo = sum . fmap (bool 0 1 . isXmas) . extend (\s -> if extract s == A then experiment applyContext s else Compose Nothing) . view asFocusedGrid
  where
    applyContext :: Coord '[Periodic n, Periodic n] -> ContextTwo (Coord '[Periodic n, Periodic n])
    applyContext c' =
      withDict (plusNat @n @1) . withDict (plusMonotone2 @n @0 @1) $
        withDict (leTrans @1 @n @(n + 1)) $
          let c :: Coord '[Periodic (n + 1), Periodic (n + 1)] = strengthenCoord c'
           in Compose $
                traverse weakenCoord $
                  Pair
                    (Two (c .+^ (-1, -1)) (c .+^ (1, 1)))
                    (Two (c .+^ (-1, 1)) (c .+^ (1, -1)))

    isXmas :: ContextTwo Cell -> Bool
    isXmas (Compose (Just (Pair l r))) = on (&&) (`elem` [Two M S, Two S M]) l r
    isXmas _ = False
