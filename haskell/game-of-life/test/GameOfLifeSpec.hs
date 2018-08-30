module GameOfLifeSpec
  ( spec
  ) where

import           Test.Hspec        (Spec, context, describe, it, shouldBe)
import           Test.Hspec.Runner (hspec)

data CellState
  = Living
  | Dead
  deriving (Show, Eq)

nextState :: CellState -> Int -> CellState
nextState currentState 2 = currentState
nextState _ 3            = Living
nextState _ _            = Dead

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Game of Life" $ do
    describe "Next generation rules" $ do
      let livingCell = Living
      let deadCell = Dead
      context "Living Cell" $ do
        it "should die on less than 2 living neighbours" $ do
          nextState livingCell 0 `shouldBe` Dead
          nextState livingCell 1 `shouldBe` Dead
        it "should survive on 2 or 3 living neighbours" $ do
          nextState livingCell 2 `shouldBe` Living
          nextState livingCell 3 `shouldBe` Living
        it "should die on more than 3 living neighbours" $ do
          nextState livingCell 4 `shouldBe` Dead
          nextState livingCell 8 `shouldBe` Dead
      context "Dead Cell" $ do
        it "should stay dead" $ do
          nextState deadCell 1 `shouldBe` Dead
          nextState deadCell 2 `shouldBe` Dead
          nextState deadCell 4 `shouldBe` Dead
        it "should be born on exactly 3 living neighbours" $
          nextState deadCell 3 `shouldBe` Living
