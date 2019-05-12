{-# LANGUAGE BangPatterns #-}

module Task2
  ( Point(..)
  , doubleArea
  , doubleAreaSimple
  , crossProduct
  , minus
  , perimeter
  , perimeterSimple
  , plus
  , scalarProduct
  ) where

data Point =
  Point Int
        Int

plus :: Point -> Point -> Point
plus (Point !x1 !y1) (Point !x2 !y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point !x1 !y1) (Point !x2 !y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point !x1 !y1) (Point !x2 !y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point !x1 !y1) (Point !x2 !y2) = x1 * y2 - y1 * x2

distance :: Point -> Point -> Double
distance (Point !x1 !y1) (Point !x2 !y2) =
  sqrt $
  (fromIntegral x2 - fromIntegral x1) * (fromIntegral x2 - fromIntegral x1) +
  (fromIntegral y2 - fromIntegral y1) * (fromIntegral y2 - fromIntegral y1)

perimeter :: [Point] -> Double
perimeter [] = 0
perimeter (x:xs) =
  let res' = foldr (\ !curPoint (!res, !prevPoint) -> (res + distance curPoint prevPoint, curPoint)) (0, x) xs
   in fst res' + distance x (snd res')

perimeterSimple :: [Point] -> Double
perimeterSimple [] = 0
perimeterSimple (x:xs) =
  let res' = foldr (\ !curPoint (res, prevPoint) -> (res + distance curPoint prevPoint, curPoint)) (0, x) xs
   in fst res' + distance x (snd res')

doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea (x:xs) =
  let res' = foldr (\ !curPoint (!res, !prevPoint) -> (res + crossProduct curPoint prevPoint, curPoint)) (0, x) xs
   in fst res' + crossProduct x (snd res')

doubleAreaSimple :: [Point] -> Int
doubleAreaSimple [] = 0
doubleAreaSimple (x:xs) =
  let res' = foldr (\ !curPoint (res, prevPoint) -> (res + crossProduct curPoint prevPoint, curPoint)) (0, x) xs
   in fst res' + crossProduct x (snd res')
