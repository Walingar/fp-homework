{-# LANGUAGE NumericUnderscores #-}

module Task2Benchmark
  ( pointsBenchmark
  ) where

import Task2 (Point (..), doubleArea, perimeter)

import Criterion.Main (bench, bgroup, defaultMain, nf)

generatePoints :: Int -> [Point]
generatePoints n = replicate n (Point 0 0)

pointsBenchmark :: IO ()
pointsBenchmark = defaultMain [bgroup "perimenter bench" [perimeterBench], bgroup "doubleArea bench" [doubleAreaBench]]
  where
    n = 10_000_000 :: Int
    points = generatePoints n
    perimeterBench = bench "fast " $ nf perimeter points
    doubleAreaBench = bench "fast " $ nf doubleArea points
