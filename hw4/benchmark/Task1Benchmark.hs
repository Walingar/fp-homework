module Task1Benchmark
  ( multiplyBenchmark
  ) where

import Task1 (multiplySimple, multiplySimpleStrict, multiplyVector)

import Criterion.Main (bench, bgroup, defaultMain, nf)

hugeMatrix :: Int -> [[Int]]
hugeMatrix n =
  let line = replicate n 0
   in replicate n line

multiplyBenchmark :: IO ()
multiplyBenchmark =
  defaultMain
    [bgroup "multiply" [multiplyStrict], bgroup "multiply" [multiplyVector'], bgroup "multiply" [multiplySimple']]
  where
    n = 500 :: Int
    matrix = hugeMatrix n
    multiplyStrict = bench "strict simple multiply " $ nf (multiplySimpleStrict matrix) matrix
    multiplyVector' = bench "vector multiply " $ nf (multiplyVector matrix) matrix
    multiplySimple' = bench "simple multiply " $ nf (multiplySimple matrix) matrix
