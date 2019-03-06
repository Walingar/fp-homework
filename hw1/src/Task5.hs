{-# LANGUAGE InstanceSigs #-}

module Task5
  ( eitherConcat
  , fromString
  , maybeConcat
  , toString
  ) where

-- |
-- >>> maybeConcat [Just [1,2,3], Nothing, Just [4,5]]
-- [1,2,3,4,5]
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr f []
  where
    f :: Maybe [a] -> [a] -> [a]
    f Nothing list    = list
    f (Just cur) list = cur ++ list

-- |
-- >>> eitherConcat [Left [3], Right [1,2,3], Left [5], Right [4,5]]
-- ([3,5],[1,2,3,4,5])
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldr f (mempty, mempty)
  where
    f :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
    f (Left left) (a, b)   = (left <> a, b)
    f (Right right) (a, b) = (a, right <> b)

data ThisOrThat a b
  = This a
  | That b
  | Both a
         b

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) (This a) (This b)         = This (a <> b)
  (<>) (This a) (That b)         = Both a b
  (<>) (That a) (This b)         = Both b a
  (<>) (That a) (That b)         = That (a <> b)
  (<>) (This a) (Both x y)       = Both (a <> x) y
  (<>) (That b) (Both x y)       = Both x (b <> y)
  (<>) (Both x y) (This a)       = Both (x <> a) y
  (<>) (Both x y) (That b)       = Both x (y <> b)
  (<>) (Both x1 y1) (Both x2 y2) = Both (x1 <> x2) (y1 <> y2)

newtype Name =
  Name String
  deriving (Show)

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (<>) (Name "") (Name b) = Name b
  (<>) (Name a) (Name "") = Name a
  (<>) (Name a) (Name b)  = Name (a ++ ('.' : b))

-- |
-- >>> Name "root" <> Name "server"
-- Name "root.server"
-- >>> Name "" <> Name "server"
-- Name "server"
instance Monoid Name where
  mempty :: Name
  mempty = Name ""

newtype Endo a = Endo
  { getEndo :: a -> a
  }

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) Endo {getEndo = a} Endo {getEndo = b} = Endo $ a . b

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id

data Builder
  = One Char
  | Many [Builder]

instance Semigroup Builder where
  (<>) :: Builder -> Builder -> Builder
  (<>) ch1@(One _) ch2@(One _)   = Many [ch1, ch2]
  (<>) ch@(One _) (Many [])      = ch
  (<>) ch@(One _) (Many list)    = Many $ ch : list
  (<>) (Many []) ch@(One _)      = ch
  (<>) (Many list) ch@(One _)    = Many $ list ++ [ch]
  (<>) (Many list1) (Many list2) = Many $ list1 ++ list2

instance Monoid Builder where
  mempty :: Builder
  mempty = Many []

fromString :: String -> Builder
fromString = foldr f mempty
  where
    f :: Char -> Builder -> Builder
    f current builder = One current <> builder

-- |
-- >>> toString $ fromString "check this functions"
-- "check this functions"
-- >>> toString $ fromString ""
-- ""
toString :: Builder -> String
toString (One ch)    = [ch]
toString (Many list) = foldr f "" list
  where
    f :: Builder -> String -> String
    f builder st = toString builder ++ st
