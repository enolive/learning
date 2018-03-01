module FizzBuzz
    ( generateFor
    ) where

import Data.List (intercalate)

generateFor :: Int -> String
generateFor input = interHyphen input [result | (appliesTo, result) <- rules, appliesTo input]

rules :: [(Int -> Bool, String)]
rules = [
  (numbersDivisibleBy 3, "Fizz"),
  (numbersDivisibleBy 5, "Buzz")
  ]

numbersDivisibleBy :: Int -> Int -> Bool
numbersDivisibleBy divisor number = number `mod` divisor == 0

interHyphen :: Int -> [String] -> String
interHyphen defaultValue [] = show defaultValue
interHyphen _ xs = intercalate "-" xs