module Main where

import Lib

type InitialValue = Int
type Deltas = [Int]
type Absolutes = [Int]

deltasToAbsolutes :: InitialValue -> Deltas -> Absolutes
deltasToAbsolutes _ [] = []
deltasToAbsolutes lastSum (x:xs) = (x + lastSum) : deltasToAbsolutes (x + lastSum) xs 

main :: IO ()
main = someFunc
