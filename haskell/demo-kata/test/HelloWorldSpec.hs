module HelloWorldSpec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (hspec)
import HelloWorld (greet, greetWorld)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Hello World" $ do
    it "should greet the specified person" $
      greet "Christoph" `shouldBe` "Hello, Christoph!"
    it "should greet the world" $
      greetWorld `shouldBe` "Hello, World!"