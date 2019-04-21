module GrammarExpressions
  ( AssignmentData(..)
  , Code(..)
  , ValueBuilder(..)
  , VarName(..)
  , VarValue(..)
  ) where

newtype VarName =
  VarName String
  deriving (Show)

newtype VarValue =
  VarValue [ValueBuilder]
  deriving (Show)

data AssignmentData =
  AssignmentData VarName
                 VarValue
  deriving (Show)

data ValueBuilder
  = ValueBuilderElement String
  | Argument String
  deriving (Show)

newtype Code =
  Code [AssignmentData]
