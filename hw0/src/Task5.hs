module Task5
  ( churchMult
  , churchPlus
  , churchToInt
  , intToChurch
  , succChurch
  , zero
  ) where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch f g y = f g (g y)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus f x g y = f g (x g y)

churchMult :: Nat a -> Nat a -> Nat a
churchMult f x g = f (x g)

churchToInt :: Nat Integer -> Integer
churchToInt f = f (+ 1) 0

intToChurch :: Integer -> Nat Integer
intToChurch y = \f x -> intToChurch' f x y
  where
    intToChurch' :: (Integer -> Integer) -> Integer -> Integer -> Integer
    intToChurch' _ x 0 = x
    intToChurch' f x n = f $ intToChurch' f x (n - 1)
