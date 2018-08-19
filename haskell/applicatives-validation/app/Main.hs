module Main where

import Data.List
import Data.Validation
import Lib

type FromAddress = Address

type ToAddress = Address

newtype Address =
  Address String
  deriving (Show)

newtype Body =
  Body String
  deriving (Show)

data Error = EmptyBody | AddressMustContain String deriving (Show)

data Email =
  Email FromAddress
        ToAddress
        Body
  deriving (Show)

main :: IO ()
main = do
  print $ validateEmail "from" "to" ""
  print $ validateEmail "from@mail" "to@mail" "This is a test"

validateAddress :: String -> Validation [Error] Address
validateAddress address
  | "@" `isInfixOf` address = Address <$> Success address
  | otherwise = Failure [AddressMustContain "@"]

validateBody :: String -> Validation [Error] Body
validateBody [] = Failure [EmptyBody]
validateBody body = Body <$> Success body

validateEmail :: String -> String -> String -> Validation [Error] Email
validateEmail from to body = Email
  <$> validateAddress from
  <*> validateAddress to
  <*> validateBody body