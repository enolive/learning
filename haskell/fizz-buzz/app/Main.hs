module Main where

import Data.List
import FizzBuzz

main :: IO ()
main = mapM_ (putStrLn . generateFor) [1 .. 100]