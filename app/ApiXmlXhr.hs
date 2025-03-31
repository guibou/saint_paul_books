{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ApiXmlXhr where

import Api
import ApiRequest
import Books
import Control.Concurrent.Async (async)
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Reflex.Dom

refreshBook :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m)) => (Text -> IO ()) -> Event t User -> m (Event t (Either Text (UTCTime, [Book])))
refreshBook pushLog userEvent = performEventAsync $ (refreshBooksCallback pushLog) <$> userEvent

refreshBooksCallback :: (MonadIO m) => (Text -> IO ()) -> User -> (Either Text (UTCTime, [Book]) -> IO ()) -> m ()
refreshBooksCallback pushLog credential callback = do
  let logWithName t = pushLog (displayName credential <> ": " <> t)
  void $ liftIO $ async $ do
    resM <- try $ refreshBooks logWithName credential
    res <- case resM of
      Right res -> pure res
      Left (err :: SomeException) -> do
        pushLog "done error"
        pure $ Left (Text.pack $ show err)

    callback res
  pure ()

refreshBooks :: (Text -> IO ()) -> User -> IO (Either Text (UTCTime, [Book]))
refreshBooks pushLog User {..} = do
  let logSection section action = do
        let start = do
              pushLog ("starting " <> section)
              t <- getCurrentTime
              pure t
            end t = do
              t' <- getCurrentTime
              let deltaTime = t' `diffUTCTime` t
              pushLog ("ending " <> section <> " in " <> Text.pack (show deltaTime))
        bracket start end (const action)
  logSection "refreshBooks" $ do
    session <- logSection "getIguanaSession" getIguanaSession
    auth <- logSection "getLogin" $ getLogin session credential
    items <- logSection "getLoan" $ getLoan auth
    case eitherDecode @[JSONBook] (encode items) of
      Right books -> do
        mtime <- getCurrentTime
        pure $ Right (mtime, coerce books)
      Left err -> pure (Left $ Text.pack err)
