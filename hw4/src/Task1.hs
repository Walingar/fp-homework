module Task1
  ( multiply
  , multiplySimple
  ) where

import Data.List (transpose)
import Control.Monad.ST
import Data.Array.ST

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply a b =
  if not (checkMatrix a b)
    then Nothing
    else Just $ multiply' a b

type IntArray s = STUArray s Int Int

type Matrix s  = STUArray s Int (ST s (IntArray s))

toIntArray :: [Int] -> ST s (IntArray s)
toIntArray list = do
  let listSize = length list
  newListArray (0, listSize - 1) list

toMatrix :: [[Int]] -> ST s (STUArray s Int (ST s (IntArray s)))
toMatrix list = do
  let listSize = length list
  k <- newListArray (0, listSize - 1) (map toIntArray list)
  undefined

multiply' :: [[Int]] -> [[Int]] -> [[Int]]
multiply' a b = runST $ do
  let n = length a
  let k = length $ head b
  a' <- toMatrix a
  b' <- toMatrix b
  undefined

checkMatrix :: [[Int]] -> [[Int]] -> Bool
checkMatrix a b
  | null a || null b = False
  | null (head a) || null (head b) = False
  | length (head a) /= length b = False
  | otherwise = True

multiplySimple :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplySimple a b =
  if not (checkMatrix a b)
    then Nothing
    else Just [[sum $ zipWith (*) a' b' | b' <- transpose b] | a' <- a]
