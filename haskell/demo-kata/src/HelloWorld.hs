module HelloWorld
  ( greet
  , defaultPerson
  , name
  ) where

newtype Person = Person
  { name :: String
  }

defaultPerson :: Person
defaultPerson = Person {name = "World"}

greet :: Person -> String
greet p = "Hello, " ++ name p ++ "!"