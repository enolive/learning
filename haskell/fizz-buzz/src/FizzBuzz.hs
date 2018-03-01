module FizzBuzz
    ( generateFor
    ) where

generateFor :: Int -> String
generateFor n
  | n `divisibleBy` 3 = "Fizz"
  | otherwise = show n

divisibleBy :: Int -> Int -> Bool
divisibleBy n d = n `mod` d == 0