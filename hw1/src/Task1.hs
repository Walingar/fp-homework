module Task1
  ( order3
  , smartReplicate
  , contains
  , stringSum
  ) where

import Data.List (sort)

-- |
-- >>> order3 (5, 2, 10)
-- (2,5,10)
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (x, y, z)
  where
    [x, y, z] = sort [a, b, c]

-- |
-- >>> smartReplicate [1,2,3]
-- [1,2,2,3,3,3]
smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

-- |
-- >>> contains 3 [[1..5], [2,0], [3,4]]
-- [[1,2,3,4,5],[3,4]]
contains :: Ord a => a -> [[a]] -> [[a]]
contains element = filter (elem element)

-- |
-- >>> stringSum "1 1"
-- 2
-- >>> stringSum "100\n\t-3"
-- 97
-- >>> :{
-- let passTests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
--                 , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
--                 , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
--                 ]
-- :}
--
-- >>> map stringSum passTests
-- [1,6,1,1,1,12345,60,1368,-1,-6,-12345,-1368,553,400]
stringSum :: String -> Int
stringSum s = sum (map read (words s))
