module FizzBuzz
    ( generateFor
    ) where

import Data.List (intercalate)

generateFor :: Int -> String
generateFor n = interHyphen n [result | (appliesTo, result) <- rules, appliesTo n]

rules :: [(Int -> Bool, String)]
rules = [
  (numbersDivisibleBy 3, "Fizz"),
  (numbersDivisibleBy 5, "Buzz")
  ]

numbersDivisibleBy :: Int -> Int -> Bool
numbersDivisibleBy d n = n `mod` d == 0

interHyphen :: Int -> [String] -> String
interHyphen defaultValue [] = show defaultValue
interHyphen _ xs = intercalate "-" xs