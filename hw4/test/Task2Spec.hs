module Task2Spec where

import Task2 (Point (..), doubleArea, perimeter)
import Test.Hspec (Spec, it, shouldBe, shouldStartWith)

spec :: Spec
spec = do
  it "perimeter tests" $ do (show $ perimeter [Point 0 0, Point 8 2, Point (-2) 6]) `shouldStartWith` "25.34"
  it "doubleArea tests" $ do (doubleArea [Point 3 4, Point 5 6, Point 9 5, Point 12 8, Point 5 11]) `shouldBe` 60
