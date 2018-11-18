module GameOfLifeSpec
  ( spec
  ) where

import           Data.List         (delete)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Test.Hspec        (Spec, context, describe, it, shouldBe,
                                    shouldMatchList)
import           Test.Hspec.Runner (hspec)

import           GameOfLife

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Game of Life" $ do
    context "Rules for Next Generation" $ do
      context "Living Cell" $ do
        let livingCell = Living
        it "Underpopulation" $ do
          nextGen livingCell 0 `shouldBe` Dead
          nextGen livingCell 1 `shouldBe` Dead
        it "Survival" $ do
          nextGen livingCell 2 `shouldBe` Living
          nextGen livingCell 3 `shouldBe` Living
        it "Overcrowding" $ do
          nextGen livingCell 4 `shouldBe` Dead
          nextGen livingCell 8 `shouldBe` Dead
      context "Dead Cell" $ do
        let deadCell = Dead
        it "Stay Dead" $ do
          nextGen deadCell 2 `shouldBe` Dead
          nextGen deadCell 1 `shouldBe` Dead
          nextGen deadCell 4 `shouldBe` Dead
        it "Reproduction" $ nextGen deadCell 3 `shouldBe` Living
    context "Board" $ do
      let emptyBoard = mkBoard
      context "Accessing" $ do
        it "Has only dead cells" $ do
          emptyBoard `cellStateAt` (0, 0) `shouldBe` Dead
          emptyBoard `cellStateAt` (4, 1) `shouldBe` Dead
        it "Has one living cell" $ do
          let board = setStateAt emptyBoard (0, 0) Living
          board `cellStateAt` (0, 0) `shouldBe` Living
        it "Has multiple living cells" $ do
          let board = setMultipleAlive emptyBoard [(0, 0), (1, 1), (2, 2)]
          board `cellStateAt` (0, 0) `shouldBe` Living
          board `cellStateAt` (1, 1) `shouldBe` Living
          board `cellStateAt` (2, 2) `shouldBe` Living
          board `cellStateAt` (4, 12) `shouldBe` Dead
        it "Can kill a living cell" $ do
          let board = setMultipleAlive emptyBoard [(0, 0)]
          let killCell = setStateAt board (0, 0) Dead
          killCell `cellStateAt` (0, 0) `shouldBe` Dead
        it "Setting Alive is idempotent" $ do
          let board = setMultipleAlive emptyBoard [(0, 0), (0, 0), (0, 0)]
          let killCell = setStateAt board (0, 0) Dead
          killCell `cellStateAt` (0, 0) `shouldBe` Dead
        it "Killing is idempotent" $ do
          let board = setMultipleAlive emptyBoard [(0, 0), (1, 1)]
          let killCells = setMultipleDead board [(0, 0), (0, 0), (1, 1)]
          killCells `cellStateAt` (0, 0) `shouldBe` Dead
          killCells `cellStateAt` (1, 1) `shouldBe` Dead
      context "Counting Neighbours" $ do
        it "Counts empty board" $ countNeighboursOf emptyBoard (0, 0) `shouldBe` 0
        it "Counts one living cell" $ do
          let board = setMultipleAlive emptyBoard [(0, 0)]
          countNeighboursOf board (1, 1) `shouldBe` 1
        it "Ignores cell itself" $ do
          let board = setMultipleAlive emptyBoard [(0, 0), (1, 1)]
          countNeighboursOf board (1, 1) `shouldBe` 1
        it "Counts all adjacent cells" $ do
          let board =
                setMultipleAlive emptyBoard [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2)]
          countNeighboursOf board (1, 1) `shouldBe` 8
          countNeighboursOf board (2, 2) `shouldBe` 3
      context "Affected Cells" $ do
        it "None Affected" $ affectedCells emptyBoard `shouldBe` []
        it "Single Living Cell" $ do
          let board = setMultipleAlive emptyBoard [(1, 1)]
          affectedCells board `shouldMatchList` [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2)]
        it "Multiple Living Cells" $ do
          let board = setMultipleAlive emptyBoard [(1, 1), (3, 3)]
          affectedCells board `shouldMatchList`
            [ (0, 0)
            , (1, 0)
            , (2, 0)
            , (0, 1)
            , (1, 1)
            , (2, 1)
            , (0, 2)
            , (1, 2)
            , (2, 2)
            , (3, 2)
            , (4, 2)
            , (2, 3)
            , (3, 3)
            , (4, 3)
            , (2, 4)
            , (3, 4)
            , (4, 4)
            ]
      context "Initializing Board" $ do
        it "Empty" $
          fromLines [] `shouldBe` mkBoard
        it "Single Alive" $ do
          let initialized = fromLines [".X."]
          let expected = setMultipleAlive mkBoard [(1, 0)]
          initialized `shouldBe` expected
        it "Multiple Alive" $ do
          let initialized = fromLines [".X.", ".X.", ".X."]
          let expected = setMultipleAlive mkBoard [(1, 0), (1, 1), (1, 2)]
          initialized `shouldBe` expected
      context "Playing Game" $ do
        it "Oscillator" $ do
          let verticalBar = fromLines [".X.", ".X.", ".X."]
          let horizontalBar = fromLines ["...", "XXX", "..."]
          nextGenBoard verticalBar `shouldBe` horizontalBar
          nextGenBoard horizontalBar `shouldBe` verticalBar
        it "Square" $ do
          let square = fromLines ["....", ".XX.", ".XX.", "...."]
          nextGenBoard square `shouldBe` square
        it "Diamond" $ do
          let diamond = fromLines [".X.", "X.X", ".X."]
          nextGenBoard diamond `shouldBe` diamond
        it "Single" $ do
          let single = fromLines ["...", ".X.", "..."]
          let empty = fromLines []
          nextGenBoard single `shouldBe` empty
