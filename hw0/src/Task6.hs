module Task6
  ( x1
  , x2
  , foo
  ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

-- (Left ("harold" ++ " hide " ++ "the " ++ "pain")), Left ("harold" ++ " hide " ++ "the " ++ "pain")))
x1 = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- False
x2 = null $ mapMaybe foo "pole chudes ochen' chudesno"

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing
