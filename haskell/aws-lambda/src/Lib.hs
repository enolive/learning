{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import           Aws.Lambda
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.HashMap.Lazy          (HashMap)
import qualified Data.HashMap.Lazy          as HashMap
import           GHC.Generics

data Event = Event
    { resource       :: String
    , pathParameters :: Maybe (HashMap String String)
    }
    deriving (Generic, FromJSON)

data Response = Response
    { statusCode :: Int
    , body       :: String
    }
    deriving (Generic, ToJSON)

handler :: Event -> Context -> IO (Either String Response)
handler Event {..} context =
  pure $ Right Response {statusCode = 200, body = (greet . nameOrDefault "No Pants") pathParameters}

nameOrDefault :: String -> Maybe (HashMap String String) -> String
nameOrDefault defaultName Nothing = defaultName
nameOrDefault defaultName (Just params) = HashMap.lookupDefault defaultName "name" params

greet :: String -> String
greet name = "Hello, " <> name <> "!"
