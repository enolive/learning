{-# LANGUAGE RecordWildCards #-}

module GameOfLife
  ( CellState(..)
  , Board
  , Position
  , nextState
  , mkBoard
  , changeStateOfCellAt
  , stateOfCellAt
  , countNeighboursOf
  , affectedCellsOn
  ) where

import           Data.Set (Set)
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
stateOfCellAt Board {..} position
  | position `Set.member` livingCells = Living
  | otherwise = Dead

changeStateOfCellAt :: CellState -> Board -> Position -> Board
changeStateOfCellAt Living Board {..} position = Board {livingCells = Set.insert position livingCells}
changeStateOfCellAt Dead Board {..} position = Board {livingCells = Set.delete position livingCells}

countNeighboursOf :: Board -> Position -> Int
countNeighboursOf board position = length livingNeighbours
  where
    livingNeighbours = filter isLiving candidates
    isLiving position = board `stateOfCellAt` position == Living
    candidates = neighboursOf position

neighboursOf :: Position -> [Position]
neighboursOf (x, y) = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x,y+1), (x+1, y+1)]

affectedCellsOn :: Board -> [Position]
affectedCellsOn Board{..}
  | null livingCells = []
  | otherwise = first : neighboursOf first
  where
    first = (head . Set.toList) livingCells