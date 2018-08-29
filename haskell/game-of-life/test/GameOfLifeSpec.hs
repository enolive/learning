module GameOfLifeSpec
  ( spec
  ) where

import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.Hspec.Runner (hspec)

data CellState = Living | Dead deriving (Show, Eq)

nextState :: CellState -> Int -> CellState
nextState currentState livingNeighbours = Dead

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