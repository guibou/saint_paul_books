{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Refresh where

import Api
import Control.Concurrent.Async (forConcurrently)
import Data.Aeson
import Data.Text (Text)
import Data.Traversable (for)
import GHC.Generics
import System.Process (callProcess)

data User = User
  { user :: Text,
    password :: Text,
    name :: String
  }
  deriving (FromJSON, Generic, Show)

main = do
  Just users <- decodeFileStrict @[User] "credentials.json"
  forConcurrently users $ \User {..} -> do
    auth <- login (Credential {..})
    items <- getLoan auth
    encodeFile ("result_" <> name <> ".json") items
