import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Runner (hspec)

import FizzBuzz (generateFor)

main :: IO ()
main =
  hspec $
  describe "Fizz-Buzz Generator" $ do
    it "should return normal numbers as is" $ do
      generateFor 1 `shouldBe` "1"
      generateFor 2 `shouldBe` "2"
    it "should return numbers divisible by 3 as Fizz" $ do
      generateFor 3 `shouldBe` "Fizz"
      generateFor 6 `shouldBe` "Fizz"
    it "should return numbers divisible by 5 as Buzz" $ do
      generateFor 5 `shouldBe` "Buzz"
      generateFor 10 `shouldBe` "Buzz"
    it "should return numbers divisible by 3 and 5 as Fizz-Buzz" $ do
      generateFor 15 `shouldBe` "Fizz-Buzz"
      generateFor 30 `shouldBe` "Fizz-Buzz"