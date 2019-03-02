module Task6 where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

x1 = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

x2 = null $ mapMaybe foo "pole chudes ochen' chudesno"

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing
