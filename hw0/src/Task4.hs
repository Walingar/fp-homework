module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function (fix)

iterateElement :: a -> [a]
iterateElement = undefined

fibonacci :: Integer -> Integer
fibonacci = fix fibonacci'
  where
    fibonacci' :: (Integer -> Integer) -> Integer -> Integer
    fibonacci' f n =
      case n of
        0 -> 1
        1 -> 1
        _ -> f (n - 1) + f (n - 2)

factorial :: Integer -> Integer
factorial = fix factorial'
  where
    factorial' :: (Integer -> Integer) -> Integer -> Integer
    factorial' f n =
      if n <= 1
        then 1
        else n * f (n - 1)

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix map'
  where
    map' :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    map' _ _ []       = []
    map' rec f (x:xs) = f x : rec f xs
