module HelloWorldSpec
  ( spec
  ) where

import HelloWorld (defaultPerson, greet, name)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Hello World Greeter" $ do
    it "should greet the specified person" $ do
      greet defaultPerson {name = "Christoph"} `shouldBe` "Hello, Christoph!"
      greet defaultPerson {name = "Chris"} `shouldBe` "Hello, Chris!"
    it "should greet the world" $ greet defaultPerson `shouldBe` "Hello, World!"