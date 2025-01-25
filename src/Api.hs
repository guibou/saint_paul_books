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

import Control.Concurrent.Async (forConcurrently)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client (CookieJar)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
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
  deriving (FromJSON, ToJSON, Generic, Show, Eq)

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
  { sessionId :: Text,
    session :: Session,
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

-- | Log to the library. Returns an 'Auth' which is valid for a few time (not
-- clear how much).
getLogin :: Credential -> IO Auth
getLogin Credential {..} = do
  --
  -- See
  -- https://groups.google.com/g/yesodweb/c/7Lwzl2fvsZY/m/NjVpqGk1KlIJ?pli=1
  -- for a discussion about not certified CA
  manager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  let httpConfig = defaultHttpConfig {httpConfigAltManager = Just manager}

  -- Step 1: get the first session id and associated cookie jar
  --
  response <- runReq httpConfig $ do
    req
      POST
      (iguana_root /: "www.main.cls")
      NoReqBody
      bsResponse
      mempty

  let cookies = responseCookieJar response
  let sessionId = extractSessionId (responseBody response)

  -- Step 2: login
  response <- runReq httpConfig $ do
    req
      POST
      (iguana_root /: "Rest.Server.cls")
      ( ReqBodyJson $
          object
            [ "request"
                .= object
                  -- Note: most of these keys appears in the query I reverse
                  -- engineered. I'm too lazy to try to remove some of them.
                  [ "language" .= ("fre" :: Text),
                    "serviceProfile" .= ("Iguana" :: Text),
                    "locationProfile" .= ("" :: Text),
                    "user" .= login,
                    "password" .= password,
                    "institution" .= ("" :: Text)
                  ]
            ]
      )
      jsonResponse
      ( "sessionId" =: sessionId
          <> "method" =: ("user/credentials" :: Text)
          <> cookieJar cookies
      )
  let Response session = responseBody response :: Response Session
  pure $ Auth {..}

-- | Returns all the book owned by an authenticated user
getLoan :: Auth -> IO [Value]
getLoan Auth {..} = do
  manager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  let httpConfig = defaultHttpConfig {httpConfigAltManager = Just manager}

  response <- runReq httpConfig $ do
    req
      POST
      (iguana_root /: "Rest.Server.cls")
      ( ReqBodyJson $
          object
            [ "request"
                .= object
                  [ "sessionId" .= let Session {..} = session in sessionId,
                    -- Note: most of these keys appears in the query I reverse
                    -- engineered. I'm too lazy to try to remove some of them.
                    "LocationProfile" .= ("" :: Text),
                    "range" .= object ["from" .= (1 :: Int), "to" .= (10 :: Int)],
                    "sort" .= object ["sortBy" .= ("!" :: Text), "sortDirection" .= ("ASC" :: Text)]
                  ]
            ]
      )
      jsonResponse
      ( "sessionId" =: sessionId
          <> "method" =: ("user/loans" :: Text)
          <> cookieJar cookies
      )
  pure $ (let (Response (Items items)) = responseBody response :: Response Items in items)

data User = User
  { credential :: Credential,
    displayName :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)

-- Refresh everything
refresh :: [User] -> IO [(Text, [Value])]
refresh users = do
  forConcurrently users $ \User {..} -> do
    auth <- getLogin credential
    items <- getLoan auth
    -- TODO: handle error here
    pure (displayName, items)
