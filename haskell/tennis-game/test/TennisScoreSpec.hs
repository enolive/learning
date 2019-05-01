{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}

module TennisScoreSpec
  ( spec
  ) where

import           TennisScore
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary GameState where
  arbitrary = oneof [runningWith <$> arbitrary <*> arbitrary, Win <$> arbitrary, Advantage <$> arbitrary, return Deuce]
    where
      runningWith x y = Running (x, y)

instance Arbitrary PlayerScore where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary WhichPlayer where
  arbitrary = arbitraryBoundedEnum

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Score for the Tennis Game" $ do
    context "construction" $ it "should produce an empty instance" $ newGame `shouldBe` Running (Love, Love)
    context "swapping" $ do
      it "should be its own inversion" $ property $ shouldBe <$> swapState . swapState <*> id
      it "should swap running score" $ property $ \x y -> swapState (Running (x, y)) `shouldBe` Running (y, x)
      it "should swap a win" $ property $ \player -> swapState (Win player) `shouldBe` (Win . swapPlayer) player
      it "should swap an advantage" $
        property $ \player -> swapState (Advantage player) `shouldBe` (Advantage . swapPlayer) player
    context "simple ops" $ do
      it "should score for player 1" $ newGame `wonPoint` Player1 `shouldBe` Running (Fifteen, Love)
      it "should score for player 2" $ newGame `wonPoint` Player2 `shouldBe` Running (Love, Fifteen)
      it "should work for all possible states" $ property $ \state player -> shouldNotThrow $ state `wonPoint` player
      it "should never produce Forty, Forty" $
        property $ \list -> newGame `wonPoints` list `shouldNotBe` Running (Forty, Forty)
    context "list ops" $
      it "should execute wonPoint more than once" $
      newGame `wonPoints` [Player1, Player2, Player1, Player2] `shouldBe` Running (Thirty, Thirty)
    context "running" $ do
      it "should score player twice" $ do
        newGame `wonPoints` replicate 2 Player1 `shouldBe` Running (Thirty, Love)
        newGame `wonPoints` replicate 2 Player2 `shouldBe` Running (Love, Thirty)
      it "should keep the score of the losing player" $ do
        newGame `wonPoints` [Player2, Player1] `shouldBe` Running (Fifteen, Fifteen)
        newGame `wonPoints` [Player1, Player2] `shouldBe` Running (Fifteen, Fifteen)
      it "should let player score three points" $ do
        newGame `wonPoints` replicate 3 Player1 `shouldBe` Running (Forty, Love)
        newGame `wonPoints` replicate 3 Player2 `shouldBe` Running (Love, Forty)
    context "winning" $ do
      it "should let player with 4 points and opponent with at most two points win" $
        property $
        forAll (suchThat arbitrary (isWinningSituationFor Player1)) $ \list ->
          newGame `wonPoints` list `shouldBe` Win Player1
      it "keeps a win a win" $ property $ \player list -> Win player `wonPoints` list `shouldBe` Win player
    context "deucing" $ do
      it "should let players with 3 points deuce" $
        property $ forAll (suchThat arbitrary isDeuceSituation) $ \list -> newGame `wonPoints` list `shouldBe` Deuce
      it "should let player advantage" $ property $ \player -> Deuce `wonPoint` player `shouldBe` Advantage player
    context "advantage" $ do
      it "should let advantage go lost" $
        property $ \player -> Advantage player `wonPoint` swapPlayer player `shouldBe` Deuce
      it "should let advantage result in win" $
        property $ \player -> Advantage player `wonPoint` player `shouldBe` Win player
    context "showing all possible states" $ do
      it "should show running game" $ do
        wonPoints newGame [Player1] `shouldShowAs` "15 - 0"
        wonPoints newGame [Player1, Player1] `shouldShowAs` "30 - 0"
        wonPoints newGame [Player1, Player1, Player1] `shouldShowAs` "40 - 0"
      it "should show player 1 winning" $
        property $
        forAll (suchThat arbitrary (isWinningSituationFor Player1)) $ \list ->
          wonPoints newGame list `shouldShowAs` "Player 1 wins"
      it "should show player 2 winning" $
        property $
        forAll (suchThat arbitrary (isWinningSituationFor Player2)) $ \list ->
          wonPoints newGame list `shouldShowAs` "Player 2 wins"
      it "should show deuce" $
        property $ forAll (suchThat arbitrary isDeuceSituation) $ \list -> wonPoints newGame list `shouldShowAs` "Deuce"
      it "should show advantage for player 1" $
        wonPoints newGame [Player1, Player1, Player2, Player2, Player1, Player2, Player1] `shouldShowAs`
        "Advantage for Player 1"
      it "should show advantage for player 2" $
        wonPoints newGame [Player2, Player1, Player2, Player2, Player1, Player1, Player2] `shouldShowAs`
        "Advantage for Player 2"
      it "should never show an empty instance" $ property $ \state -> show (state :: GameState) `shouldNotBe` ""

isWinningSituationFor :: WhichPlayer -> [WhichPlayer] -> Bool
isWinningSituationFor winner xs = countPlayer winner xs >= 4 && countPlayer (swapPlayer winner) xs <= 2

isDeuceSituation :: [WhichPlayer] -> Bool
isDeuceSituation xs = countPlayer Player1 xs == 3 && countPlayer Player2 xs == 3

countPlayer :: WhichPlayer -> [WhichPlayer] -> Int
countPlayer player = count (== player)

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

shouldShowAs :: Show a => a -> String -> Expectation
shouldShowAs obj expected = show obj `shouldBe` expected

shouldNotThrow :: Show a => a -> Expectation
shouldNotThrow expr = expr `shouldSatisfy` anyValue

anyValue :: a -> Bool
anyValue = const True
