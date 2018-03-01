module FizzBuzz
    ( generateFor
    ) where

import Data.List (intercalate)

rules :: [(Int -> Bool, String)]
rules = [
  (numbersDivisibleBy 3, "Fizz"),
  (numbersDivisibleBy 5, "Buzz")
  ]

numbersDivisibleBy :: Int -> Int -> Bool
numbersDivisibleBy d n = n `mod` d == 0

generateFor :: Int -> String
generateFor n = interHyphen n [result | (appliesTo, result) <- rules, appliesTo n]

interHyphen :: Int -> [String] -> String
interHyphen defaultValue [] = show defaultValue
interHyphen _ xs = intercalate "-" xs