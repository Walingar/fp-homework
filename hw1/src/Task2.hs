module Task2
  ( mergeSort
  , randomIntList
  , remove
  ) where

import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

-- |
-- >>> remove 3 [1, 2, 3, 4, 5]
-- ([1,2,3,5],Just 4)
-- >>> remove 6 [1, 2, 3, 4, 5]
-- ([1,2,3,4,5],Nothing)
-- >>> remove 0 [1, 2, 3, 4, 5]
-- ([2,3,4,5],Just 1)
remove :: Int -> [a] -> ([a], Maybe a)
remove ind list = remove' (splitAt ind list)
  where
    remove' :: ([a], [a]) -> ([a], Maybe a)
    remove' (x, [])   = (x, Nothing)
    remove' (x, y:ys) = (x ++ ys, Just y)

-- |
-- >>> import Data.List (sort)
-- >>> example <- randomIntList 20 (-10) 10
-- >>> let exampleSorted = sort example
-- >>> exampleSorted == (mergeSort example)
-- True
mergeSort :: Ord a => [a] -> [a]
mergeSort []   = []
mergeSort [a]  = [a]
mergeSort list = mergeImpl (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length list `div` 2) list
    mergeImpl :: Ord a => [a] -> [a] -> [a]
    mergeImpl [] xs = xs
    mergeImpl xs [] = xs
    mergeImpl (x:xs) (y:ys)
      | x <= y    = x : mergeImpl xs (y : ys)
      | otherwise = y : mergeImpl (x : xs) ys
