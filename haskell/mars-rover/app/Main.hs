module Main where

import MarsRover
import Control.Monad

main :: IO ()
main = do
  putStrLn "enter commands for the rover, <EOF> to quit"
  loop mkRover

loop initRover = displayRover initRover >> getNewRover initRover >>= loop

displayRover = print
getNewRover rover = commands rover <$> getLine

