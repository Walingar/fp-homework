module Main where

import Task2Benchmark (pointsBenchmark)

main :: IO ()
main = do
--  multiplyBenchmark
  pointsBenchmark
