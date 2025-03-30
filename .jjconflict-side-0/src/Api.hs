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
import qualified Data.ByteString.Char8 as BS
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Network.HTTP.Client (CookieJar)
import Network.HTTP.Req

-- | Represents the root of the iguana library
--
-- In the future this could be set as an API parameter so it may query other
-- iguana libraries
iguana_root :: Url Https
iguana_root = https "mediatheques-saintpaul.re" /: "iguana"

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
    cookies :: CookieJar
  }
  deriving (Show)

-- | Actually, we deserialize the Item query as Value to keep the complete
-- output payload on disk, if we would like to edit the display and use more
-- fields.
data Items = Items
  { items :: [Value]
  }
  deriving (Show, FromJSON, Generic)

extractSessionId :: _ -> Text
extractSessionId t = do
  let (_, rest) = BS.breakSubstring sessionIdPrefix t
  decodeUtf8 $ BS.takeWhile (/= '\'') $ BS.drop 1 $ BS.dropWhile (/= '\'') rest
  where
    sessionIdPrefix = "Vfocus.Settings.sessionID = '"

data User = User
  { credential :: Credential,
    displayName :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)
