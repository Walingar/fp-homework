{-# LANGUAGE InstanceSigs #-}

module Task4
  ( splitOn
  , joinWith
  ) where

-- TODO: tests
data Pair a =
  Pair a
       a
  deriving (Show)

data NonEmpty a =
  a :| [a]
  deriving (Show)

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair a b) = f a <> f b
  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair a b) = f a (f b z)

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (a :| as) = f a `mappend` foldMap f as
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (a :| as) = f a (foldr f z as)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn delimiter = foldr (join delimiter) ([] :| [])
  where
    join :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    join delimiterToken current (x :| xs)
      | current == delimiterToken = [] :| (x : xs)
      | otherwise = (current : x) :| xs

joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith delimiter = foldr (f delimiter) []
  where
    f :: Eq a => a -> [a] -> [a] -> [a]
    f _ current []                = current
    f delimiterToken current list = current ++ (delimiterToken : list)
