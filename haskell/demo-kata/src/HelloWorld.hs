module HelloWorld
  ( greet
  , greetWorld
  ) where

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

greetWorld :: String
greetWorld = greet "World"