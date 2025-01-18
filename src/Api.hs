{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-
 - Api for iguana library
 -}
module Api where

import Control.Concurrent.Async (forConcurrently)
import Control.Lens (ix, (^..))
import Control.Lens.Regex.ByteString
import Data.Aeson
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Network.HTTP.Client (CookieJar)
import Network.HTTP.Req

sessionIdRegex :: _
sessionIdRegex = [regex|Vfocus.Settings.sessionID = '(.*)';|]

-- | Represents the root of the iguana library
--
-- In the future this could be set as an API parameter so it may query other
-- iguana libraries
iguana_root :: Url Https
iguana_root = https "mediatheques-saintpaul.re" /: "iguana"

-- | represents credentials for login
data Credential = Credential
  { user :: Text,
    password :: Text
  }

-- A response in the iguana api, contains a payload
data Response t = Payload {response :: t}
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

-- | Log to the library. Returns an 'Auth' which is valid for a few time (not
-- clear how much).
login :: Credential -> IO Auth
login credential = do
  -- Step 1: get the first session id and associated cookie jar
  response <- runReq defaultHttpConfig $ do
    req
      POST
      (iguana_root /: "www.main.cls")
      NoReqBody
      bsResponse
      mempty

  let cookies = responseCookieJar response
  let [decodeUtf8 -> sessionId] = (responseBody response) ^.. sessionIdRegex . groups . ix 0

  -- Step 2: login
  response <- runReq defaultHttpConfig $ do
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
                    "user" .= credential.user,
                    "password" .= credential.password,
                    "institution" .= ("" :: Text)
                  ]
            ]
      )
      jsonResponse
      ( "sessionId" =: sessionId
          <> "method" =: ("user/credentials" :: Text)
          <> cookieJar cookies
      )
  let session = (responseBody response :: Response Session).response
  pure $ Auth {..}

-- | Returns all the book owned by an authenticated user
getLoan :: Auth -> IO [Value]
getLoan Auth {..} = do
  response <- runReq defaultHttpConfig $ do
    req
      POST
      (iguana_root /: "Rest.Server.cls")
      ( ReqBodyJson $
          object
            [ "request"
                .= object
                  [ "sessionId" .= session.sessionId,
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
  pure $ (responseBody response :: Response Items).response.items

data User = User
  { user :: Text,
    password :: Text,
    name :: String
  }
  deriving (FromJSON, Generic, Show)

-- Refresh everything
refresh :: [User] -> IO [(String, [Value])]
refresh users = do
  forConcurrently users $ \User {..} -> do
    auth <- login (Credential {..})
    items <- getLoan auth
    -- TODO: handle error here
    pure (name, items)
