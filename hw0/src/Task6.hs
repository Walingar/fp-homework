module Task6
  ( foo
  , x1
  , x2
  ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

-- (Left ("harold" ++ " hide " ++ "the " ++ "pain")), Left ("harold" ++ " hide " ++ "the " ++ "pain")))
x1 :: (Either String b, Either String c)
x1 = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- False
x2 :: Bool
x2 = null $ mapMaybe foo "pole chudes ochen' chudesno"

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing
