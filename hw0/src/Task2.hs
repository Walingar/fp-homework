module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg a throw = throw a

contr :: (a -> b) -> (Neg b -> Neg a)
contr f g a = g $ f a

ax6 :: a -> Either a b
ax6 = Left

ax7 :: b -> Either a b
ax7 = Right

ax9 :: (a -> b) -> (a -> Neg b) -> Neg a
ax9 f g a = g a (f a)

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = ax9 (contr ax6) (contr ax7)

-- can't proof it.
-- Kripke structure where is no [((a -> b) -> a) -> a]:
-- w1 -> w2; w1: b
-- w2: a
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- can't proof it.
-- Kripke structure where is no [Neg (Neg a) -> a]:
-- w1 -> w2; w1 -> w3; w1: !!a
-- w2: a
-- w3: a
-- so w1 doesn't have !!a -> a because it doesn't have a
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = contr doubleNeg
