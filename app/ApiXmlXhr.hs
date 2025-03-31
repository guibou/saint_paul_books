{-# LANGUAGE FlexibleContexts #-}
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

refreshBook :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m)) => Event t User -> m (Event t (Either Text (UTCTime, [Book])))
refreshBook userEvent = performEventAsync $ refreshBooksCallback <$> userEvent

refreshBooksCallback :: (MonadIO m) => User -> (Either Text (UTCTime, [Book]) -> IO ()) -> m ()
refreshBooksCallback credential callback = do
  void $ liftIO $ async $ do
    resM <- try $ refreshBooks  credential
    res <- case resM of
      Right res -> pure res
      Left (err :: SomeException) -> do
        pure $ Left (Text.pack $ show err)

    callback res
  pure ()

refreshBooks :: User -> IO (Either Text (UTCTime, [Book]))
refreshBooks User {..} = do
  session <- getIguanaSession
  auth <- getLogin session credential
  items <- getLoan auth
  -- TODO: handle error here
  case eitherDecode @[JSONBook] (encode items) of
    Right books -> do
      mtime <- getCurrentTime
      pure $ Right (mtime, coerce books)
    Left err -> pure (Left $ Text.pack err)
