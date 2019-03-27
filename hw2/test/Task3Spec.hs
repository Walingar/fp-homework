module Task3Spec where

import Task3
import Test.Hspec

spec :: Spec
spec = do
  it "CBS tests" $ do
    isCBS "" `shouldBe` True
    isCBS "()" `shouldBe` True
    isCBS "((()))(())()" `shouldBe` True
    isCBS ")" `shouldBe` False
    isCBS "(()" `shouldBe` False
    isCBS "(()))" `shouldBe` False
    isCBS "((()))(" `shouldBe` False
    isCBS "((())))" `shouldBe` False
  it "Int parser tests" $ do
    parseInt "5" `shouldBe` Just 5
    parseInt "+5" `shouldBe` Just 5
    parseInt "-5" `shouldBe` Just (-5)
    parseInt "+00005" `shouldBe` Just 5
    parseInt "-00005" `shouldBe` Just (-5)
    parseInt "329378935" `shouldBe` Just 329378935
    parseInt "-329378935" `shouldBe` Just (-329378935)
    parseInt "32937c8935" `shouldBe` Nothing
    parseInt "--5" `shouldBe` Nothing
  it "Int list parser tests" $ do
    parseIntList "2, 1,+10  , 3,5,-7, 2" `shouldMatchList` [[1, 10], [5, -7, 2]]
    parseIntList "2, 1,+10  , 0003,5,-7, 2" `shouldMatchList` [[1, 10], [5, -7, 2]]
    parseIntList "     2,      1,       -10  ,    0003,    5,   -7,    2   " `shouldMatchList` [[1, -10], [5, -7, 2]]
    parseIntList "1, 1" `shouldMatchList` [[1]]
    parseIntList "2,1,+10, 10,5,-7,2, 2,1,2, 3,1,2,3" `shouldMatchList` [[1, 10], [5,-7,2, 2,1,2, 3,1,2,3]]
