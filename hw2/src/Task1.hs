{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes   #-}

module Task1
  ( stringSum
  , Tree(..)
  , NonEmpty(..)
  ) where

import Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum st = collect $ traverse readMaybe (words st)
  where
    collect :: Maybe [Int] -> Maybe Int
    collect (Just a) = Just $ sum a
    collect Nothing  = Nothing

data Tree a
  = Branch (Tree a)
           (Tree a)
  | Leaf a
  deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x)            = Leaf $ f x
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f) tree            = fmap f tree
  (<*>) (Branch left right) tree = Branch (left <*> tree) (right <*> tree)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf x)            = f x
  foldMap f (Branch left right) = foldMap f left <> foldMap f right

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x)            = Leaf <$> f x
  traverse f (Branch left right) = Branch <$> traverse f left <*> traverse f right

data NonEmpty a =
  a :| [a]
  deriving (Show)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (a :| list) = f a :| (f <$> list)

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| fs) (x :| xs) = f x :| ((f <$> xs) ++ (fs <*> xs))

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (a :| as) = f a <> foldMap f as

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (a :| list) = (:|) <$> f a <*> traverse f list

instance Monad NonEmpty where
  (>>=) :: forall a b. NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| xs) f = getFirst (f x) :| (getList (f x) ++ foldr toList [] (f <$> xs))
    where
      getFirst :: NonEmpty a -> a
      getFirst (a :| _) = a
      getList :: NonEmpty a -> [a]
      getList (_ :| list) = list
      toList :: NonEmpty a -> [a] -> [a]
      toList (a :| t) list = a : (t ++ list)
