module Task2
  ( ArithmeticError(..)
  , Expr(..)
  , Operation(..)
  , QueueDouble(..)
  , eval
  , moving
  ) where

import Control.Monad.State (State, execState, state)

data Operation
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  deriving (Show)

data Expr
  = Const Int
  | BinaryOp Operation
             Expr
             Expr
  deriving (Show)

data ArithmeticError
  = DivisionByZero
  | NegativePow
  deriving (Show, Eq)

eval :: Expr -> Either ArithmeticError Int
eval (Const a) = Right a
eval (BinaryOp operation left right) = eval left >>= (\x -> eval right >>= eval' operation x)
  where
    eval' :: Operation -> Int -> Int -> Either ArithmeticError Int
    eval' Add x y = Right $ x + y
    eval' Sub x y = Right $ x - y
    eval' Mul x y = Right $ x * y
    eval' Div x y
      | y == 0 = Left DivisionByZero
      | otherwise = Right $ x `div` y
    eval' Pow x y
      | y < 0 = Left NegativePow
      | otherwise = Right $ x ^ y

data QueueDouble =
  QueueDouble [Double]
              [Double]
              Int
              Double
  deriving (Show)

push :: Double -> Int -> State (QueueDouble, [Double]) ()
push e maxLen = state $ \st -> ((), push' e st)
  where
    push' :: Double -> (QueueDouble, [Double]) -> (QueueDouble, [Double])
    push' x (this@(QueueDouble st1 st2 len s), ans)
      | len == maxLen =
        let (_, queue) = pop' this
         in push' x (queue, ans)
      | otherwise = (QueueDouble (x : st1) st2 (len + 1) (s + x), ((s + x) / fromIntegral (len + 1)) : ans)

pop' :: QueueDouble -> (Double, QueueDouble)
pop' (QueueDouble st1 [] len s)     = pop' $ QueueDouble [] (reverse st1) len s
pop' (QueueDouble st1 (x:xs) len s) = (x, QueueDouble st1 xs (len - 1) (s - x))

emptyQueueDouble :: QueueDouble
emptyQueueDouble = QueueDouble [] [] 0 0

execList :: Int -> [Double] -> State (QueueDouble, [Double]) ()
execList _ [] = return ()
execList k (x:xs) = do
  push x k
  execList k xs

moving :: Int -> [Double] -> [Double]
moving k list = reverse $ snd $ execState (execList k list) (emptyQueueDouble, [])
