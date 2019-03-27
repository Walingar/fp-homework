module Task2Spec where

import Task2
import Test.Hspec

spec :: Spec
spec =
  it "eval tests" $ do
    eval (BinaryOp Add (Const 2) (Const (-1))) `shouldBe` Right 1
    eval (BinaryOp Div (Const 2) (Const (-1))) `shouldBe` Right (-2)
    eval (BinaryOp Mul (Const (-5)) (Const 2)) `shouldBe` Right (-10)
    eval (BinaryOp Div (Const (-5)) (Const 0)) `shouldBe` Left DivisionByZero
    eval (BinaryOp Pow (Const 5) (Const (-1))) `shouldBe` Left NegativePow
    eval (BinaryOp Pow (BinaryOp Div (Const 2) (Const (-1))) (BinaryOp Add (Const 2) (Const (-1)))) `shouldBe`
      Right (-2)
