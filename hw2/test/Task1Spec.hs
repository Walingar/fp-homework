module Task1Spec where

import Task1
import Test.Hspec

spec :: Spec
spec =
  it "string sum tests" $ do
    stringSum "1 2 3" `shouldBe` Just 6
    stringSum "    1     2     5      " `shouldBe` Just 8
    stringSum "    -1     -2     5      " `shouldBe` Just 2
    stringSum "    a     -2     5      " `shouldBe` Nothing
    stringSum " 4 kek it is broken test 4" `shouldBe` Nothing
