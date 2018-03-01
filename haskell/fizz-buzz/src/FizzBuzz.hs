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
generateFor n
  | null resultList = show n
  | otherwise = intercalate "-" resultList
  where
    resultList = map getResult . filter (`appliesTo` n) $ rules
    getResult = snd
    appliesTo (a, _) = a
