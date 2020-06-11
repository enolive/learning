module Lib where

import Aws.Lambda
import Data.Aeson
import GHC.Generics

data Person
  = Person
      { personName :: String,
        personAge :: Int
      }
  deriving (Generic)

instance FromJSON Person

instance ToJSON Person

handler :: Person -> Context -> IO (Either String Person)
handler person@Person {..} context
  | personAge >= 0 = return $ Right person
  | otherwise = return $ Left "The age must be greater or equal zero"
