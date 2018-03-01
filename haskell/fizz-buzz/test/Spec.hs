import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Runner (hspec)

import Lib (generateFor)

main :: IO ()
main = hspec $
  describe "Fizz-Buzz Generator" $ do
    it "should return normal numbers as is" $
      generateFor 1 `shouldBe` "1"