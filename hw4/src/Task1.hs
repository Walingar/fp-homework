{-# LANGUAGE BangPatterns #-}

module Task1
  ( multiply
  , multiplySimple
  , multiplySimpleStrict
  , multiplyVector
  ) where

import Control.Monad.ST (ST, runST)
import Data.Foldable (forM_)
import Data.List (transpose)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MU

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply = multiplySimple

multiplyVector :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyVector a b =
  if not (checkMatrix a b)
    then Nothing
    else Just $ multiply' a b

type IntArray s = MU.MVector s Int

type Matrix s = M.MVector s (IntArray s)

toIntArray :: [Int] -> ST s (IntArray s)
toIntArray list = VU.thaw $ VU.fromList list

toMatrix :: [[Int]] -> ST s (Matrix s)
toMatrix matrix = do
  lists <- traverse toIntArray matrix
  V.thaw $ V.fromList lists

emptyMatrix' :: Int -> Int -> ST s (Matrix s)
emptyMatrix' n k =
  let line = replicate k 0
   in toMatrix (replicate n line)

intArrayToList :: IntArray s -> ST s [Int]
intArrayToList list = do
  freeze <- VU.freeze list
  return $ VU.toList freeze

fromMatrix :: M.MVector s (IntArray s) -> ST s [[Int]]
fromMatrix list = do
  freeze <- V.freeze list
  list' <- traverse intArrayToList freeze
  return $ V.toList list'

multiply' :: [[Int]] -> [[Int]] -> [[Int]]
multiply' a b =
  runST $ do
    let l = length a
    let m = length $ head a
    let n = length $ head b
    a' <- toMatrix a
    b' <- toMatrix b
    c <- emptyMatrix' l n
    forM_ [0 .. l - 1] $ \i -> do
      aRow <- M.read a' i
      forM_ [0 .. n - 1] $ \j ->
        forM_ [0 .. m - 1] $ \r -> do
          bRow <- M.read b' r
          !aElement <- MU.read aRow r
          !bElement <- MU.read bRow j
          cRow <- M.read c i
          MU.modify cRow (+ (aElement * bElement)) j
    fromMatrix c

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
    else Just [[sum $ zipWith (*) a' b' | b' <- transposedB] | a' <- a]
  where
    transposedB = transpose b

multiplySimpleStrict :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplySimpleStrict a b =
  if not (checkMatrix a b)
    then Nothing
    else Just [[sum $ zipWith (*) a' b' | !b' <- transposedB] | !a' <- a]
  where
    transposedB = transpose b
