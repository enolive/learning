module Main where

import           Control.Monad
import           TennisScore
import           Text.Read

data Command
  = Exit
  | Ignore
  | Play WhichPlayer

main :: IO ()
main = putStrLn "enter player number to score or blank to exit" >> gameLoop newGame

gameLoop :: GameState -> IO ()
gameLoop state = print state >> getLine >>= readCommand >>= loopOrExit
  where
    loopOrExit Exit          = putStrLn "bye!"
    loopOrExit Ignore        = putStrLn "wrong input!" >> gameLoop state
    loopOrExit (Play player) = gameLoop (state `wonPoint` player)

readCommand :: String -> IO Command
readCommand [] = return Exit
readCommand line =
  case maybePlayer line of
    (Just player) -> return (Play player)
    _             -> return Ignore
  where
    maybePlayer = readMaybe >=> toPlayer
    toPlayer :: Int -> Maybe WhichPlayer
    toPlayer 1 = Just Player1
    toPlayer 2 = Just Player2
    toPlayer _ = Nothing
