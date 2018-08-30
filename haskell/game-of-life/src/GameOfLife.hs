{-# LANGUAGE RecordWildCards #-}

module GameOfLife
    ( CellState(..)
    , nextState
    , mkBoard
    , changeStateOfCellAt
    , stateOfCellAt
    , countNeighboursOf
    ) where

import Data.Set (Set)
import qualified Data.Set as Set

data CellState
  = Living
  | Dead
  deriving (Show, Eq)

newtype Board = Board
  { livingCells :: Set Position
  } deriving (Show)

type Position = (Int, Int)

nextState :: CellState -> Int -> CellState
nextState currentState 2 = currentState
nextState _ 3            = Living
nextState _ _            = Dead

mkBoard :: Board
mkBoard = Board {livingCells = Set.empty}

stateOfCellAt :: Board -> Position -> CellState
stateOfCellAt Board{..} position
  | position `Set.member` livingCells = Living
  | otherwise = Dead

changeStateOfCellAt :: Board -> Position -> CellState -> Board
changeStateOfCellAt Board{..} position Living = Board {livingCells = Set.insert position livingCells}
changeStateOfCellAt Board{..} position Dead   = Board {livingCells = Set.delete position livingCells}

countNeighboursOf :: Board -> Position -> Int
countNeighboursOf board position = 0

