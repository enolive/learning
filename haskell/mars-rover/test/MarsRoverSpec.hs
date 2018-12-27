module MarsRoverSpec
  ( spec
  ) where

import           MarsRover
import           Test.Hspec        (Spec, context, describe, it, shouldBe)
import           Test.Hspec.Runner (hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Mars Rover" $ do
    let rover = mkRover
    context "created instance" $ do
      it "has expected initial position" $ position rover `shouldBe` (0, 0)
      it "has expected initial facing" $ facing rover `shouldBe` North
    context "standing still" $ it "does nothing when no commands are received" $ commands rover "" `shouldBe` rover
    context "receiving single command" $ do
      context "forward" $
        it "moves forward one field in the direction it is facing" $ do
          commands rover "f" `shouldBe` rover {position = (0, 1)}
          commands rover {position = (21, 43)} "f" `shouldBe` rover {position = (21, 44)}
          commands rover {facing = West} "f" `shouldBe` rover {position = (1, 0), facing = West}
          commands rover {position = (1, 2), facing = West} "f" `shouldBe` rover {position = (2, 2), facing = West}
          commands rover {position = (1, 2), facing = South} "f" `shouldBe` rover {position = (1, 1), facing = South}
          commands rover {position = (1, 2), facing = East} "f" `shouldBe` rover {position = (0, 2), facing = East}
      context "backward" $
        it "moves backward one field opposite to the direction it is facing" $ do
          commands rover "b" `shouldBe` rover {position = (0, -1)}
          commands rover {position = (21, 43)} "b" `shouldBe` rover {position = (21, 42)}
          commands rover {facing = West} "b" `shouldBe` rover {position = (-1, 0), facing = West}
          commands rover {position = (1, 2), facing = West} "b" `shouldBe` rover {position = (0, 2), facing = West}
          commands rover {position = (1, 2), facing = South} "b" `shouldBe` rover {position = (1, 3), facing = South}
          commands rover {position = (1, 2), facing = East} "b" `shouldBe` rover {position = (2, 2), facing = East}
      context "left" $
        it "turns left by 90 degrees" $ do
          commands rover "l" `shouldBe` rover {facing = West}
          commands rover {facing = West} "l" `shouldBe` rover {facing = South}
          commands rover {facing = South} "l" `shouldBe` rover {facing = East}
          commands rover {facing = East} "l" `shouldBe` rover {facing = North}
      context "right" $
        it "turns right by 90 degrees" $ do
          commands rover "r" `shouldBe` rover {facing = East}
          commands rover {facing = East} "r" `shouldBe` rover {facing = South}
          commands rover {facing = South} "r" `shouldBe` rover {facing = West}
          commands rover {facing = West} "r" `shouldBe` rover {facing = North}
    context "multiple commands" $
      it "executes the commands from left to right" $ do
        commands rover "ff" `shouldBe` rover {position = (0, 2)}
        commands rover "ffr" `shouldBe` rover {position = (0, 2), facing = East}
        commands rover "ffrbbb" `shouldBe` rover {position = (3, 2), facing = East}
        commands rover "ffrbbbllff" `shouldBe` rover {position = (5, 2), facing = West}
    context "wrong commands" $
      it "ignores anything other than fblr" $ do
        commands rover "aaaa" `shouldBe` rover
        commands rover "wrong command that also includes flbr" `shouldBe` rover
