{-# LANGUAGE RecordWildCards #-}

module HelloWorld
  ( greet
  , defaultPerson
  , name
  ) where

import Control.Monad
import Data.List
import Data.Set ()
import qualified Data.Set as Set

newtype Person = Person
  { name :: String
  }

defaultPerson :: Person
defaultPerson = Person {name = "World"}

greet :: Person -> String
greet Person {..} = "Hello, " ++ name ++ "!"