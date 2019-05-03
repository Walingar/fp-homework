module Task1Spec where

import Task1 (multiplySimple)
import Test.Hspec

spec :: Spec
spec = do
  it "checkMatrix tests" $ do
     ([] `multiplySimple` [[-123, -12, 2], [23, 4, 1], [-2, -13, 2], [3, 1, 4]]) `shouldBe` Nothing
     ([[1, 7, 56, 2], [19, -2, 12, -1]] `multiplySimple` []) `shouldBe` Nothing
     ([[1, 7, 56, 2], [19, -2, 12, -1]] `multiplySimple` [[-123, -12, 2]]) `shouldBe` Nothing
  it "simplemultiply tests" $
     ([[1, 7, 56, 2], [19, -2, 12, -1]] `multiplySimple` [[-123, -12, 2], [23, 4, 1], [-2, -13, 2], [3, 1, 4]]) `shouldBe` Just [[-68, -710, 129], [-2410, -393, 56]]
