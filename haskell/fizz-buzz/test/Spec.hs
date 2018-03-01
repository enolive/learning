import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Runner (hspec)

import FizzBuzz (generateFor)

main :: IO ()
main = hspec $
  describe "Fizz-Buzz Generator" $ do
    it "should return normal numbers as is" $ do
      generateFor 1 `shouldBe` "1"
      generateFor 2 `shouldBe` "2"
    it "should return 3 as Fizz" $ do
      generateFor 3 `shouldBe` "Fizz"