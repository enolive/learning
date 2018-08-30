module GameOfLifeSpec
  ( spec
  ) where

import           GameOfLife
import           Test.Hspec        (Spec, context, describe, it, shouldBe)
import           Test.Hspec.Runner (hspec)

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
        it "should be born on exactly 3 living neighbours" $ nextState deadCell 3 `shouldBe` Living
    describe "Board" $ do
      let emptyBoard = mkBoard
      context "Querying" $ do
        let boardWithOneLivingCell = changeStateOfCellAt emptyBoard (1, 1) Living
        it "should have dead cells initially" $ emptyBoard `stateOfCellAt` (1, 1) `shouldBe` Dead
        it "should allow cell to be set alive" $ boardWithOneLivingCell `stateOfCellAt` (1, 1) `shouldBe` Living
        it "should allow cell to be set to dead" $ do
          let notAliveAnymore = changeStateOfCellAt boardWithOneLivingCell (1, 1) Dead
          notAliveAnymore `stateOfCellAt` (1, 1) `shouldBe` Dead
        it "should memorize position of living cell" $ boardWithOneLivingCell `stateOfCellAt` (0, 0) `shouldBe` Dead
        it "should memorize position of many living cells" $ do
          let boardWithTwoLivingCells = changeStateOfCellAt boardWithOneLivingCell (0, 0) Living
          boardWithTwoLivingCells `stateOfCellAt` (1, 1) `shouldBe` Living
          boardWithTwoLivingCells `stateOfCellAt` (0, 0) `shouldBe` Living
      context "Counting Neighbours" $ do
        it "should count on empty board" $
          emptyBoard `countNeighboursOf` (1, 1) `shouldBe` 0
