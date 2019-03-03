{-# LANGUAGE InstanceSigs #-}

module Task3
  ( nextDay
  , afterDays
  , isWeekend
  , daysToParty
  , buildCastle
  , buildEducationStructure
  , buildHouse
  , lordIsComing
  , buildTheWallAndHoldTheDoor
  , natToInt
  , intToNat
  , natSum
  , natMul
  , natSub
  , natEq
  , natCompare
  , natIsEven
  , natDiv
  , natMod
  , Tree(..)
  , treeIsEmpty
  , treeSize
  , treeFind
  , treeContains
  , treeAdd
  , treeFromList
  , treeRemove
  ) where

import Data.Foldable (Foldable (..))

data Day
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Show)

-- |
-- >>> nextDay Mon
-- Tue
-- >>> nextDay Sun
-- Mon
-- >>> nextDay Fri
-- Sat
nextDay :: Day -> Day
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

-- |
-- >>> afterDays Mon 7
-- Mon
-- >>> afterDays Sun 7
-- Sun
-- >>> afterDays Mon 8
-- Tue
afterDays :: Day -> Int -> Day
afterDays day 0 = day
afterDays day n = nextDay $ afterDays day (n - 1)

-- |
-- >>> isWeekend Mon
-- False
-- >>> isWeekend Sun
-- True
-- >>> isWeekend Sat
-- True
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

-- |
-- >>> daysToParty Sat
-- 6
-- >>> daysToParty Mon
-- 4
-- >>> daysToParty Fri
-- 0
daysToParty :: Day -> Int
daysToParty Fri = 0
daysToParty day = 1 + daysToParty (nextDay day)

newtype CastleWithLord =
  CastleWithLord Bool

newtype CityWall =
  CityWall Bool

data EducationStructure
  = Church
  | Library
  deriving (Show)

data Family
  = One
  | Two
  | Three
  | Four

familyToInt :: Family -> Int
familyToInt One   = 1
familyToInt Two   = 2
familyToInt Three = 3
familyToInt Four  = 4

data HouseList
  = FirstHouse Family
  | House Family
          HouseList

data City =
  City (Maybe (CastleWithLord, CityWall)) -- if it is Nothing, there is no Castle.
       (Maybe EducationStructure)
       HouseList

buildCastle :: City -> Either String City
buildCastle (City Nothing educationStructure houses) =
  Right $ City (Just (CastleWithLord False, CityWall False)) educationStructure houses
buildCastle _ = Left "Can't build castle because it built yet"

buildEducationStructure :: City -> EducationStructure -> Either String City
buildEducationStructure (City castle Nothing houses) structure = Right $ City castle (Just structure) houses
buildEducationStructure (City _ existStructure _) structure =
  Left $ "Can't build " ++ show structure ++ " because " ++ show existStructure ++ " built yet"

buildHouse :: City -> Family -> City
buildHouse (City castle educationStructure houses) family = City castle educationStructure (House family houses)

countPeople :: HouseList -> Int
countPeople (FirstHouse family)       = familyToInt family
countPeople (House family housesTail) = familyToInt family + countPeople housesTail

lordIsComing :: City -> Either String City
lordIsComing = undefined

buildTheWallAndHoldTheDoor :: City -> Either String City
buildTheWallAndHoldTheDoor (City (Just (CastleWithLord True, CityWall False)) educationStructure houses) =
  if countPeople houses >= 10
    then Right $ City (Just (CastleWithLord True, CityWall True)) educationStructure houses
    else Left "Can't build the wall because there are not enough people"
buildTheWallAndHoldTheDoor _ = Left "Can't build the wall because there is no Lord"

data Nat
  = Z
  | S Nat
  deriving (Show)

-- |
-- >>> natToInt (S (S Z))
-- 2
natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S a) = 1 + natToInt a

-- |
-- >>> intToNat 4
-- S (S (S (S Z)))
intToNat :: Int -> Nat
intToNat x
  | x < 0 = error "Expected argument > 0"
  | x == 0 = Z
  | otherwise = S $ intToNat (x - 1)

-- $setup
-- >>> let checkNatBiFunction f x y = natToInt $ f (intToNat x) (intToNat y)
--
-- |
-- >>> checkNatBiFunction natSum 10 24
-- 34
natSum :: Nat -> Nat -> Nat
natSum Z b     = b
natSum (S a) b = S $ natSum a b

-- |
-- >>> checkNatBiFunction natMul 11 24
-- 264
natMul :: Nat -> Nat -> Nat
natMul Z _     = Z
natMul (S a) b = natSum b (natMul a b)

-- |
-- >>> checkNatBiFunction natSub 11 24
-- 0
-- >>> checkNatBiFunction natSub 24 11
-- 13
natSub :: Nat -> Nat -> Nat
natSub Z _         = Z
natSub a Z         = a
natSub (S a) (S b) = natSub a b

-- |
-- >>> natCompare (intToNat 11) (intToNat 24)
-- -1
-- >>> natCompare (intToNat 11) (intToNat 11)
-- 0
-- >>> natCompare (intToNat 24) (intToNat 11)
-- 1
natCompare :: Nat -> Nat -> Int
natCompare Z Z         = 0
natCompare Z _         = -1
natCompare _ Z         = 1
natCompare (S a) (S b) = natCompare a b

-- |
-- >>> natEq (intToNat 11) (intToNat 24)
-- False
-- >>> natEq (intToNat 11) (intToNat 11)
-- True
natEq :: Nat -> Nat -> Bool
natEq a b = natCompare a b == 0

-- |
-- >>> natIsEven $ intToNat 11
-- False
-- >>> natIsEven $ intToNat 24
-- True
natIsEven :: Nat -> Bool
natIsEven Z         = True
natIsEven (S Z)     = False
natIsEven (S (S a)) = natIsEven a

-- |
-- >>> checkNatBiFunction natDiv 48 24
-- 2
-- >>> checkNatBiFunction natDiv 49 24
-- 2
-- >>> checkNatBiFunction natDiv 47 24
-- 1
-- >>> checkNatBiFunction natDiv 23 24
-- 0
natDiv :: Nat -> Nat -> Nat
natDiv _ Z = error "Expected non-zero second argument"
natDiv a b
  | natCompare a b == -1 = Z
  | otherwise = S $ natDiv (natSub a b) b

-- |
-- >>> checkNatBiFunction natMod 48 24
-- 0
-- >>> checkNatBiFunction natMod 49 24
-- 1
-- >>> checkNatBiFunction natMod 47 24
-- 23
-- >>> checkNatBiFunction natMod 23 24
-- 23
natMod :: Nat -> Nat -> Nat
natMod a b
  | natCompare a b == -1 = a
  | otherwise = natSub a (natMul (natDiv a b) b)

data Tree a
  = Leaf
  | Node [a]
         (Tree a)
         (Tree a)
  deriving (Show)

treeFromList :: Ord a => [a] -> Tree a
treeFromList []     = Leaf
treeFromList (x:xs) = treeAdd (treeFromList xs) x

-- |
-- >>> toList $ treeFromList [2, 1, 5, 19, -1]
-- [-1,1,2,5,19]

-- |
-- >>> treeSize $ treeFromList [2, 1, 5, 19, -1]
-- 5
treeSize :: Tree a -> Int
treeSize Leaf                   = 0
treeSize (Node list left right) = length list + treeSize left + treeSize right

-- |
-- >>> treeIsEmpty $ treeFromList []
-- True
-- >>> treeIsEmpty $ treeFromList [1, 3, 2]
-- False
treeIsEmpty :: Tree a -> Bool
treeIsEmpty tree = treeSize tree == 0

-- |
-- >>> treeContains (treeFromList [2, 1, 5, 19, -1]) 1
-- True
-- >>> treeContains (treeFromList [2, 1, 5, 19, -1]) 0
-- False
treeContains :: Ord a => Tree a -> a -> Bool
treeContains tree value =
  case treeFind tree value of
    Nothing -> False
    _       -> True

treeFind :: Ord a => Tree a -> a -> Maybe (Tree a)
treeFind Leaf _ = Nothing
treeFind (Node [] _ _) _ = error "Unexpected empty list in Node"
treeFind node@(Node (x:_) left right) value
  | x == value = Just node
  | x > value = treeFind left value
  | otherwise = treeFind right value

treeAdd :: Ord a => Tree a -> a -> Tree a
treeAdd Leaf value = Node [value] Leaf Leaf
treeAdd (Node [] _ _) _ = error "Unexpected empty list in Node"
treeAdd (Node (x:xs) left right) value
  | x == value = Node (value : x : xs) left right
  | x > value = Node (x : xs) (treeAdd left value) right
  | otherwise = Node (x : xs) left (treeAdd right value)

-- |
-- >>> let (ans, ok) = treeRemove (treeFromList [2, 1, 5, 19, -1]) 5
--
-- >>> toList $ ans
-- [-1,1,2,19]
treeRemove :: Ord a => Tree a -> a -> (Tree a, Bool)
treeRemove Leaf _ = (Leaf, False)
treeRemove (Node [] _ _) _ = error "Unexpected empty list in Node"
treeRemove (Node (x:xs) left right) value
  | x == value =
    case xs of
      [] ->
        case right of
          Leaf -> (left, True)
          _ ->
            let (removed, tree) = treeMinRemove right
             in (Node removed left tree, True)
      _ -> (Node xs left right, True)
  | x > value =
    let (ans, ok) = treeRemove left value
     in (Node (x : xs) ans right, ok)
  | otherwise =
    let (ans, ok) = treeRemove right value
     in (Node (x : xs) left ans, ok)

treeMinRemove :: Ord a => Tree a -> ([a], Tree a)
treeMinRemove Leaf = error "Unexpected empty tree"
treeMinRemove (Node list Leaf _) = (list, Leaf)
treeMinRemove (Node list left right) = (deleted, Node list leftPrepared right)
  where
    (deleted, leftPrepared) = treeMinRemove left

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf                = mempty
  foldMap f (Node x left right) = foldMap f left <> foldMap f x <> foldMap f right
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf                = z
  foldr f z (Node k left right) = foldr f (foldr f (foldr f z right) k) left
