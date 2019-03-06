{-# LANGUAGE InstanceSigs #-}

module Task3
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay

  , CastleWithLord (..)
  , City (..)
  , CityProcessResult (..)
  , CityWall (..)
  , EducationStructure (..)
  , Family (..)
  , HouseList (..)
  , lordIsComing
  , buildCastle
  , buildEducationStructure
  , buildHouse
  , buildTheWallAndHoldTheDoor

  , Nat (..)
  , intToNat
  , natToInt
  , natDiv
  , natSum
  , natMul
  , natSub
  , natIsEven
  , natMod

  , Tree (..)
  , treeAdd
  , treeContains
  , treeFind
  , treeFromList
  , treeIsEmpty
  , treeRemove
  , treeSize
  ) where

import Task4 (NonEmpty (..))

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

data CityProcessResult
  = Success City
  | Error String

buildCastle :: City -> CityProcessResult
buildCastle (City Nothing educationStructure houses) =
  Success $ City (Just (CastleWithLord False, CityWall False)) educationStructure houses
buildCastle _ =
  Error "Can't build castle because it built yet"

buildEducationStructure :: City -> EducationStructure -> CityProcessResult
buildEducationStructure (City castle Nothing houses) structure =
  Success $ City castle (Just structure) houses
buildEducationStructure (City _ existStructure _) structure =
  Error $ "Can't build " ++ show structure ++ " because " ++ show existStructure ++ " built yet"

buildHouse :: City -> Family -> City
buildHouse (City castle educationStructure houses) family =
  City castle educationStructure (House family houses)

countPeople :: HouseList -> Int
countPeople (FirstHouse family)       = familyToInt family
countPeople (House family housesTail) = familyToInt family + countPeople housesTail

lordIsComing :: City -> CityProcessResult
lordIsComing (City Nothing _ _) =
  Error "Lord, you don't have home!"
lordIsComing (City (Just (CastleWithLord True, _)) _ _) =
  Error "Lord, you should fight for your honor!"
lordIsComing (City (Just (CastleWithLord False, wall)) educationStructure houses) =
  Success $ City (Just (CastleWithLord True, wall)) educationStructure houses

buildTheWallAndHoldTheDoor :: City -> CityProcessResult
buildTheWallAndHoldTheDoor (City (Just (CastleWithLord True, CityWall False)) educationStructure houses) =
  if countPeople houses >= 10
  then Success $ City (Just (CastleWithLord True, CityWall True)) educationStructure houses
  else Error "Can't build the wall because there are not enough people"
buildTheWallAndHoldTheDoor (City (Just (CastleWithLord False, CityWall False)) _ _) =
  Error "Can't build the wall because there is no Lord"
buildTheWallAndHoldTheDoor (City (Just (_, CityWall True)) _ _) =
  Error "Can't build the wall because it already built"
buildTheWallAndHoldTheDoor (City Nothing _ _) =
  Error "Can't build the wall because there is no Castle"

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
  | x < 0 = error "Expected argument >= 0"
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
-- >>> intToNat 11 == intToNat 24
-- False
-- >>> intToNat 11 == intToNat 11
-- True
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) a b = compare a b == EQ

-- |
-- >>> compare (intToNat 11) (intToNat 24)
-- LT
-- >>> compare (intToNat 11) (intToNat 11)
-- EQ
-- >>> compare (intToNat 24) (intToNat 11)
-- GT
instance Ord Nat where
   compare :: Nat -> Nat -> Ordering
   compare Z Z         = EQ
   compare Z _         = LT
   compare _ Z         = GT
   compare (S a) (S b) = compare a b

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
  | a < b     = Z
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
  | a < b     = a
  | otherwise = natSub a (natMul (natDiv a b) b)

data Tree a
  = Leaf
  | Node (NonEmpty a)
         (Tree a)
         (Tree a)
  deriving (Show)

-- |
-- >>> import Data.Foldable (Foldable (..))
-- >>> toList $ treeFromList [2, 1, 5, 19, -1]
-- [-1,1,2,5,19]
treeFromList :: Ord a => [a] -> Tree a
treeFromList []     = Leaf
treeFromList (x:xs) = treeAdd (treeFromList xs) x

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
treeIsEmpty Leaf = True
treeIsEmpty _    = False

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
treeFind node@(Node (x :| _) left right) value
  | x == value = Just node
  | x > value = treeFind left value
  | otherwise = treeFind right value

treeAdd :: Ord a => Tree a -> a -> Tree a
treeAdd Leaf value = Node (value :| []) Leaf Leaf
treeAdd (Node list@(x :| xs) left right) value
  | x == value = Node (value :| (x : xs)) left right
  | x > value = Node list (treeAdd left value) right
  | otherwise = Node list left (treeAdd right value)

-- |
-- >>> import Data.Foldable (Foldable (..))
-- >>> let (ans, ok) = treeRemove (treeFromList [2, 1, 5, 19, -1]) 5
--
-- >>> toList $ ans
-- [-1,1,2,19]
treeRemove :: Ord a => Tree a -> a -> (Tree a, Bool)
treeRemove Leaf _ = (Leaf, False)
treeRemove (Node list@(x :| xs) left right) value
  | x == value =
    case xs of
      [] ->
        case right of
          Leaf -> (left, True)
          _ ->
            let (removed, tree) = treeMinRemove right
            in (Node removed left tree, True)
      (y:ys) -> (Node (y :| ys) left right, True)
  | x > value  =
    let (ans, ok) = treeRemove left value
    in (Node list ans right, ok)
  | otherwise  =
    let (ans, ok) = treeRemove right value
    in (Node list left ans, ok)

treeMinRemove :: Ord a => Tree a -> (NonEmpty a, Tree a)
treeMinRemove Leaf = error "Unexpected empty tree"
treeMinRemove (Node list Leaf _)     = (list, Leaf)
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
