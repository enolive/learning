module Tennis where

data Game
  = Points (Point, Point)
  | Deuce
  | Advantage WhichPlayer
  | Win WhichPlayer
  deriving (Show, Eq)

data Point
  = Love
  | Fifteen
  | Thirty
  | Forty
  deriving (Show, Eq, Enum)

data WhichPlayer
  = Player1
  | Player2
  deriving (Show, Eq, Bounded, Enum)

mkGame :: Game
mkGame = Points (Love, Love)

scores :: [WhichPlayer] -> Game -> Game
scores ps g = foldl (flip score) g ps

score :: WhichPlayer -> Game -> Game
score Player1 (Points (Thirty, Forty)) = Deuce
score Player1 (Points (Forty, _)) = Win Player1
score Player1 (Points (s1, s2)) = Points (succ s1, s2)
score Player2 g = (flipGame . score Player1 . flipGame) g
score p Deuce = Advantage p
score p1 (Advantage p2)
  | p1 /= p2 = Deuce
  | otherwise = Win p1
score _ g@(Win _) = g

flipGame :: Game -> Game
flipGame (Points (s1, s2)) = Points (s2, s1)
flipGame (Win p) = Win $ otherPlayer p
flipGame (Advantage p) = Advantage $ otherPlayer p
flipGame g = g

otherPlayer :: WhichPlayer -> WhichPlayer
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1
