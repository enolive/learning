module TennisScore where

data GameState
  = Running PlayerScore
            PlayerScore
  | Deuce
  | Advantage WhichPlayer
  | Win WhichPlayer
  deriving (Eq)

data PlayerScore
  = Love
  | Fifteen
  | Thirty
  | Forty
  deriving (Eq, Enum, Bounded)

data WhichPlayer
  = Player1
  | Player2
  deriving (Eq, Enum, Bounded)

instance Show GameState where
  show (Running x y)      = show x ++ " - " ++ show y
  show (Win player)       = show player ++ " wins"
  show (Advantage player) = "Advantage for " ++ show player
  show Deuce              = "Deuce"

instance Show PlayerScore where
  show Love    = "0"
  show Fifteen = "15"
  show Thirty  = "30"
  show Forty   = "40"

instance Show WhichPlayer where
  show Player1 = "Player 1"
  show Player2 = "Player 2"

newGame :: GameState
newGame = Running Love Love

wonPoints :: GameState -> [WhichPlayer] -> GameState
wonPoints = foldl wonPoint

wonPoint :: GameState -> WhichPlayer -> GameState
wonPoint state Player2 = (swapState . flip wonPoint Player1 . swapState) state
wonPoint Deuce player = Advantage player
wonPoint (Advantage current) new
  | current == new = Win new
  | otherwise = Deuce
wonPoint (Win player) _ = Win player
wonPoint (Running Thirty Forty) _ = Deuce
wonPoint (Running Forty _) player = Win player
wonPoint (Running change keep) _ = Running (succ change) keep

swapState :: GameState -> GameState
swapState (Running score1 score2) = Running score2 score1
swapState (Win player)            = (Win . swapPlayer) player
swapState (Advantage player)      = (Advantage . swapPlayer) player
swapState Deuce                   = Deuce

swapPlayer :: WhichPlayer -> WhichPlayer
swapPlayer Player1 = Player2
swapPlayer Player2 = Player1
