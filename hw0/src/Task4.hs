module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import Data.Function (fix)

iterateElement :: a -> [a]
iterateElement = fix iterate'
  where
    iterate' :: (a -> [a]) -> a -> [a]
    iterate' rec cur = cur : rec cur

fibonacci :: Integer -> Integer
fibonacci = fix fibonacci'
  where
    fibonacci' :: (Integer -> Integer) -> Integer -> Integer
    fibonacci' f n
      | n < 0     = error "Expected not less than zero argument"
      | n == 0    = 1
      | n == 1    = 1
      | otherwise = f (n - 1) + f (n - 2)

factorial :: Integer -> Integer
factorial = fix factorial'
  where
    factorial' :: (Integer -> Integer) -> Integer -> Integer
    factorial' f n
      | n < 0     = error "Expected more than zero argument"
      | n == 1    = 1
      | otherwise = n * f (n - 1)

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix map'
  where
    map' :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    map' _ _ []       = []
    map' rec f (x:xs) = f x : rec f xs
