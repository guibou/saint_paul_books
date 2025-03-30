{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ApiRequest where

import Api
import Control.Concurrent.Async (forConcurrently)
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import Network.HTTP.Req

newHttpConfig :: IO HttpConfig
newHttpConfig = do
  --
  -- See
  -- https://groups.google.com/g/yesodweb/c/7Lwzl2fvsZY/m/NjVpqGk1KlIJ?pli=1
  -- for a discussion about not certified CA
  manager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  let httpConfig = defaultHttpConfig {httpConfigAltManager = Just manager}
  pure httpConfig

iguanaRequest :: (FromJSON res) => IguanaSession -> Text -> [Pair] -> IO (JsonResponse res)
iguanaRequest IguanaSession {..} method payload = do
  httpConfig <- newHttpConfig

  response <- runReq httpConfig $ do
    req
      POST
      (iguana_root /: "Rest.Server.cls")
      ( ReqBodyJson $ object ["request" .= object payload]
      )
      jsonResponse
      ( "sessionId" =: iguanaSessionId
          <> "method" =: method
          <> cookieJar cookies
      )
  pure response

-- | Step 1: get the first session id and associated cookie jar
getIguanaSession :: IO IguanaSession
getIguanaSession = do
  httpConfig <- newHttpConfig

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
  let iguanaSessionId = extractSessionId (responseBody response)
  pure IguanaSession {..}

-- | Log to the library. Returns an 'Auth' which is valid for a few time (not
-- clear how much).
getLogin :: IguanaSession -> Credential -> IO Auth
getLogin iguanaSession Credential {..} = do
  response <-
    iguanaRequest iguanaSession "user/credentials" $
      -- Note: most of these keys appears in the query I reverse
      -- engineered. I'm too lazy to try to remove some of them.
      [ "language" .= ("fre" :: Text),
        "serviceProfile" .= ("Iguana" :: Text),
        "locationProfile" .= ("" :: Text),
        "user" .= login,
        "password" .= password,
        "institution" .= ("" :: Text)
      ]
  let Response session = responseBody response :: Response Session
  pure $ Auth {..}

-- | Returns all the book owned by an authenticated user
getLoan :: Auth -> IO [Value]
getLoan Auth {..} = do
  response <-
    iguanaRequest iguanaSession "user/loans" $
      [ "sessionId" .= sessionId session,
        -- Note: most of these keys appears in the query I reverse
        -- engineered. I'm too lazy to try to remove some of them.
        "LocationProfile" .= ("" :: Text),
        "range" .= object ["from" .= (1 :: Int), "to" .= (10 :: Int)],
        "sort" .= object ["sortBy" .= ("!" :: Text), "sortDirection" .= ("ASC" :: Text)]
      ]
  pure $ (let (Response (Items items)) = responseBody response :: Response Items in items)

-- Refresh everything
refresh :: [User] -> IO [(Text, [Value])]
refresh users = do
  -- We can get the session logic before refreshing
  -- NOTE: it could be possible in theory to keep that, but it is unclear how
  -- much time the session ids and auths are kept alive by the server.
  iguanaSession <- getIguanaSession
  forConcurrently users $ \User {..} -> do
    auth <- getLogin iguanaSession credential
    items <- getLoan auth
    -- TODO: handle error here
    pure (displayName, items)
