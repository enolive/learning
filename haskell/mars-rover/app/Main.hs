module Main where

import           Control.Monad
import           MarsRover

main :: IO ()
main = displayIntro >> gameLoop mkRover

displayIntro :: IO ()
displayIntro = putStrLn "enter commands or exit with blank line"

gameLoop :: Rover -> IO ()
gameLoop initRover = print initRover >> getLine >>= loopOrBreakOnBlank initRover
  where
    loopOrBreakOnBlank rover line = unless (null line) $ gameLoop (commands rover line)
