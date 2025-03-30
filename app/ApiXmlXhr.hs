{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ApiXmlXhr where

import Api hiding (iguana_root)
import ApiRequest
import Books
import Control.Concurrent.Async (async)
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.ByteString.Char8 (toStrict)
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Debug.Trace (traceShow)
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom

refreshBook :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m), MonadJSM (Performable m)) => Event t User -> m (Event t (Either Text (UTCTime, [Book])))
refreshBook userEvent = do
  iguanaSessionE <- getIguanaSession' userEvent
  let e' = traceEvent "iguanaSession" iguanaSessionE
  performEvent (pure () <$ e')
  performEventAsync $ refreshBooksCallback <$> userEvent

refreshBooksCallback :: (MonadIO m) => User -> (Either Text (UTCTime, [Book]) -> IO ()) -> m ()
refreshBooksCallback credential callback = do
  void $ liftIO $ async $ do
    resM <- refreshBooks credential
    callback resM
  pure ()

refreshBooks :: User -> IO (Either Text (UTCTime, [Book]))
refreshBooks User {..} = do
  resM <- try $ do
    session <- getIguanaSession
    auth <- getLogin session credential
    items <- getLoan auth
    -- TODO: handle error here
    pure items

  case resM of
    Right res -> do
      -- Write the payload to the disk
      -- That's useful for debuging the payload content
      encodeFile ".iguana.json" res

      case eitherDecode @[JSONBook] (encode res) of
        Right books -> do
          mtime <- getCurrentTime
          pure $ Right (mtime, coerce books)
        Left err -> pure (Left $ Text.pack err)
    Left (err :: SomeException) -> do
      pure $ Left (Text.pack $ show err)

getIguanaSession' event = do
  -- TODO: why is this POST?
  let request = XhrRequest "POST" (iguana_root <> "/Rest.Server.cls") def
  response <- performRequestAsync (request <$ event)
  pure $ _xhrResponse_responseText <$> response

extractIguanaSession :: XhrResponse -> Maybe IguanaSession
extractIguanaSession response = do
  responseBody <- _xhrResponse_responseText response
  let headers = _xhrResponse_headers response

  -- TODO: the extract session logic could be converted to Text
  let iguanaSessionId = extractSessionId (encodeUtf8 responseBody)

  pure $ traceShow headers $ IguanaSession {..}

-- | Represents the root of the iguana library
--
-- In the future this could be set as an API parameter so it may query other
-- iguana libraries
iguana_root :: Text
iguana_root = "https://mediatheques-saintpaul.re/iguana"
