module GameOfLife
    ( CellState(..)
    , nextState
    ) where

data CellState
  = Living
  | Dead
  deriving (Show, Eq)

nextState :: CellState -> Int -> CellState
nextState currentState 2 = currentState
nextState _ 3            = Living
nextState _ _            = Dead