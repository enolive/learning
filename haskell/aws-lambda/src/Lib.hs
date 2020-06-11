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

data Response = Response {greeting :: String} deriving (Generic)

instance FromJSON Response

instance ToJSON Response

handler :: Person -> Context -> IO (Either String Response)
handler person@Person {..} context
  | personAge >= 0 =  (return . Right . Response . greet) personName
  | otherwise = return $ Left "The age must be greater or equal zero"
    
greet :: String -> String
greet personName = "Hello, " <> personName <> "!"
