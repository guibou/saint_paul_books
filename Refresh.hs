{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Refresh where

import Data.Aeson
import Data.Traversable (for)
import GHC.Generics
import System.Process (callProcess)

data User = User
  { user :: String,
    password :: String,
    name :: String
  }
  deriving (FromJSON, Generic, Show)

main = do
  Just users <- decodeFileStrict @[User] "credentials.json"
  for users $ \user -> do
    callProcess "sh" ["login_and_get.sh", user.user, user.password, user.name]
  print users
