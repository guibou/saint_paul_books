{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-
 - Api for iguana library
 -}
module Api where

import Data.Aeson
import Data.Text
import GHC.Generics

-- | represents credentials for login
data Credential = Credential
  { login :: Text,
    password :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq, Ord)

-- A response in the iguana api, contains a payload
data Response t = Response {response :: t}
  deriving (Show, FromJSON, Generic)

-- | Response from login
data Session = Session
  { sessionId :: Text,
    token :: Text
  }
  deriving (Show, FromJSON, Generic)

-- | A complete authenticated session
data Auth = Auth
  { iguanaSession :: IguanaSession,
    session :: Session
  }
  deriving (Show)

data IguanaSession = IguanaSession
  { iguanaSessionId :: Text,
    cookies :: Text
  }
  deriving (Show, Generic)


instance FromJSON IguanaSession where
  parseJSON = withObject "IguanaSession" $ \o -> do
    iguanaSessionId <- o .: "sessionId"
    cookies <- o .: "cookies"
    pure $ IguanaSession {..}

-- | Actually, we deserialize the Item query as Value to keep the complete
-- output payload on disk, if we would like to edit the display and use more
-- fields.
data Items = Items
  { items :: [Value]
  }
  deriving (Show, FromJSON, Generic)

data User = User
  { credential :: Credential,
    displayName :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)
