module Task3
  ( gauss
  , verifySolution
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Foldable (forM_)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MU

type BoolArray s = MU.MVector s Bool

type Matrix s = M.MVector s (BoolArray s)

toBoolArray :: [Bool] -> ST s (BoolArray s)
toBoolArray list = VU.thaw $ VU.fromList list

toMatrix :: [[Bool]] -> ST s (Matrix s)
toMatrix matrix = do
  lists <- traverse toBoolArray matrix
  V.thaw $ V.fromList lists

findRowToSwap :: Int -> Int -> Matrix s -> ST s Int
findRowToSwap j m a = do
  ans <- newSTRef j
  forM_ [j .. m - 1] $ \i -> do
    row <- M.read a i
    el <- MU.read row j
    when el $ writeSTRef ans i
  readSTRef ans

boolArrayToList :: BoolArray s -> ST s [Bool]
boolArrayToList list = do
  freeze <- VU.freeze list
  return $ VU.toList freeze

gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
gauss a b =
  runST $ do
    let n = length $ head a
    let m = length a
    a' <- toMatrix a
    b' <- toBoolArray b
    forM_ [0 .. m - 1] $ \i ->
      when (i < n) $ do
        toSwap <- findRowToSwap i m a'
        M.swap a' i toSwap
        MU.swap b' i toSwap
        row <- M.read a' i
        forM_ [0 .. m - 1] $ \j ->
          when (i /= j) $ do
            row' <- M.read a' j
            mainEl' <- MU.read row' i
            when mainEl' $ do
              elB' <- MU.read b' j
              elB <- MU.read b' i
              MU.write b' j (elB' /= elB)
              forM_ [i .. n - 1] $ \k -> do
                el' <- MU.read row' k
                el <- MU.read row k
                MU.write row' k (el' /= el)
    resultList <- boolArrayToList b'
    return $ Just resultList

countRow :: [Bool] -> [Bool] -> Bool
countRow [x] [y]       = x /= y
countRow (x:xs) (y:ys) = (x /= y) && countRow xs ys
countRow _ _           = error "Input data is incorrect. Expected N, N matrices."

verifySolution :: [[Bool]] -> [Bool] -> [Bool] -> Bool
verifySolution [] [] _           = True
verifySolution (a':a) (b':b) res = (countRow a' res == b') && verifySolution a b res
verifySolution _ _ _             = error "Input data is incorrect. Expected MxN, M, N matrices."
