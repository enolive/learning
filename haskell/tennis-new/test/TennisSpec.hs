{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-orphans #-}

module TennisSpec
  ( spec,
  )
where

import Tennis
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

instance Arbitrary WhichPlayer where
  arbitrary = arbitraryBoundedEnum

differentPlayers :: (WhichPlayer, WhichPlayer) -> Bool
differentPlayers (p1, p2) = p1 /= p2

spec :: Spec
spec =
  describe "Tennis" $ do
    context "initialization" $
      it "constructs (Love, Love) score" $
        mkGame `shouldBe` Points (Love, Love)
    context "simple scoring" $ do
      context "only player 1 scores" $ do
        it "1x" $
          score Player1 mkGame `shouldBe` Points (Fifteen, Love)
        it "2x" $
          scores [Player1, Player1] mkGame `shouldBe` Points (Thirty, Love)
        it "3x" $
          scores [Player1, Player1, Player1] mkGame `shouldBe` Points (Forty, Love)
        it "4x" $
          scores [Player1, Player1, Player1, Player1] mkGame `shouldBe` Win Player1
      context "only player 2 scores" $ do
        it "1x" $
          score Player2 mkGame `shouldBe` Points (Love, Fifteen)
        it "2x" $
          scores [Player2, Player2] mkGame `shouldBe` Points (Love, Thirty)
        it "3x" $
          scores [Player2, Player2, Player2] mkGame `shouldBe` Points (Love, Forty)
        it "4x" $
          scores [Player2, Player2, Player2, Player2] mkGame `shouldBe` Win Player2
      context "both players have some scores" $
        it "keeps the score of the non scoring player" $
          scores [Player2, Player1, Player2, Player1] mkGame `shouldBe` Points (Thirty, Thirty)
      context "deuce" $ do
        it "player 1 scores last" $
          score Player1 (Points (Thirty, Forty)) `shouldBe` Deuce
        it "player 2 scores second" $
          score Player2 (Points (Forty, Thirty)) `shouldBe` Deuce
      context "advantage" $ do
        it "player who scores after deuce has advantage" $
          property $ \p ->
            score p Deuce `shouldBe` Advantage p
        it "other player can deuce after an advantage" $
          property $
            forAll (suchThat arbitrary differentPlayers) $ \(p1, p2) ->
              score p1 (Advantage p2) `shouldBe` Deuce
        it "same player can win after an advantage" $
          property $ \p ->
            score p (Advantage p) `shouldBe` Win p
      context "win" $
        it "is final" $
          property $ \(p1, p2) ->
            score p1 (Win p2) `shouldBe` Win p2
