module Main where

import           Control.Monad
import           MarsRover

main :: IO ()
main = displayIntro >> gameLoop mkRover

displayIntro :: IO ()
displayIntro = putStrLn "enter commands or exit with blank line"

gameLoop :: Rover -> IO ()
gameLoop initRover = do
  print initRover
  line <- getLine
  doOver initRover line
  where
    doOver rover line = unless (null line) $ gameLoop (commands rover line)
