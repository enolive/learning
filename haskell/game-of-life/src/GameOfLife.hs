{-# LANGUAGE RecordWildCards #-}

module GameOfLife
  ( affectedCells
  , mkBoard
  , nextGenBoard
  , countNeighboursOf
  , nextGen
  , CellState(..)
  , fromLines
  , cellStateAt
  , setStateAt
  , setMultipleAlive
  , setMultipleDead
  ) where

import           Data.Maybe (mapMaybe)
import           Data.Set   (Set)
import qualified Data.Set   as Set

data CellState
  = Living
  | Dead
  deriving (Eq, Show)

newtype Board = Board
  { livingCells :: Set Position
  } deriving (Eq, Show)

type Position = (Int, Int)

mkBoard :: Board
mkBoard = Board {livingCells = Set.empty}

cellStateAt :: Board -> Position -> CellState
cellStateAt Board {..} position
  | position `elem` livingCells = Living
  | otherwise = Dead

setStateAt :: Board -> Position -> CellState -> Board
setStateAt Board {..} position Living = Board {livingCells = position `Set.insert` livingCells}
setStateAt Board {..} position Dead = Board {livingCells = position `Set.delete` livingCells}

setMultipleAlive :: Board -> [Position] -> Board
setMultipleAlive = foldl setAlive

setMultipleDead :: Board -> [Position] -> Board
setMultipleDead = foldl kill

setAlive :: Board -> Position -> Board
setAlive board position = setStateAt board position Living

kill :: Board -> Position -> Board
kill board position = setStateAt board position Dead

nextGen :: CellState -> Int -> CellState
nextGen current 2 = current
nextGen _ 3       = Living
nextGen _ _       = Dead

affectedCells :: Board -> [Position]
affectedCells Board {..} = onlyUnique $ surrounding =<< Set.elems livingCells
  where
    onlyUnique = Set.toList . Set.fromList
    surrounding position = position : neighboursOf position

neighboursOf :: Position -> [Position]
neighboursOf (x, y) =
  [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

countNeighboursOf :: Board -> Position -> Int
countNeighboursOf board position = length . filter isAlive $ candidates
  where
    candidates = neighboursOf position
    isAlive position = board `cellStateAt` position == Living

fromLines :: [String] -> Board
fromLines lines = foldl setState mkBoard cellPositions
  where
    setState board (state, pos) = setStateAt board pos state
    positions = [(x, y) | y <- [0 .. maxY - 1], x <- [0 .. maxX - 1]]
    maxY = length lines
    maxX = length . head $ lines
    cellPositions = zip cells positions
    cells = map whichState $ concat lines
    whichState '.' = Dead
    whichState 'X' = Living

nextGenBoard :: Board -> Board
nextGenBoard board = Board {livingCells = Set.fromList livingCellsInNextGen}
  where
    livingCellsInNextGen = mapMaybe toNextGen . affectedCells $ board
    toNextGen position
      | nextState position == Living = Just position
      | otherwise = Nothing
    nextState position = nextGen (board `cellStateAt` position) (board `countNeighboursOf` position)
