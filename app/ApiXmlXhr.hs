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
import GHC.IO (unsafePerformIO)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- TODO: here all the semantic is copy/pasted between the different action, and
-- the login phase is redone everytime, which maybe not optimal

refreshBook :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m)) => (Text -> IO ()) -> Event t User -> m (Event t (Either Text (UTCTime, [Book])))
refreshBook pushLog userEvent = performEventAsync $ (refreshBooksCallback pushLog) <$> userEvent

renewBookAsync :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m)) => (Text -> IO ()) -> Event t (Book, User) -> m (Event t (Either Text (UTCTime, [Book])))
renewBookAsync pushLog bookEvent = performEventAsync $ (renewBookCallback pushLog) <$> bookEvent

renewBookCallback :: (MonadIO m) => (Text -> IO ()) -> (Book, User) -> (Either Text (UTCTime, [Book]) -> IO a) -> m ()
renewBookCallback pushLog (book, user) callback = do
  let logWithName t = pushLog (displayName user <> ": " <> t)
  void $ liftIO $ async $ do
    resM <- try $ renewBook logWithName (book, user)
    res <- case resM of
      Right res -> pure res
      Left (err :: SomeException) -> do
        pushLog $ "Error:" <> (Text.pack $ show err)
        pure $ Left (Text.pack $ show err)

    callback res
  pure ()

refreshBooksCallback :: (MonadIO m) => (Text -> IO ()) -> User -> (Either Text (UTCTime, [Book]) -> IO ()) -> m ()
refreshBooksCallback pushLog user callback = do
  liftIO $ pushLog "-----------------------------------------"
  let logWithName t = pushLog (displayName user <> ": " <> t)
  void $ liftIO $ async $ do
    resM <- try $ refreshBooks logWithName user
    res <- case resM of
      Right res -> pure res
      Left (err :: SomeException) -> do
        pushLog $ "Error" <> (Text.pack $ show err)
        pushLog $ "Let's try to refresh the credentials"

        -- Let's invalide the previous identificatino token and retry
        atomically $ do
          writeTVar sessionVar NoSession
          modifyTVar authsVar (Map.delete $ credential user)

        resM <- try $ refreshBooks logWithName user
        case resM of
          Right res -> pure res
          Left (err :: SomeException) -> do
            pushLog $ "FATALError:" <> (Text.pack $ show err)

            pure $ Left (Text.pack $ show err)

    callback res
  pure ()

{-# NOINLINE sessionVar #-}
sessionVar :: TVar SessionStatus
sessionVar = unsafePerformIO $ newTVarIO NoSession

authsVar :: TVar (Map Credential Auth)
authsVar = unsafePerformIO $ newTVarIO mempty

data SessionStatus = NoSession | GettingSession | AvailableSession IguanaSession

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
    session <- logSection "getIguanaSessionCache" $ do
      sessionM <- atomically $ do
        sessionState <- readTVar sessionVar
        case sessionState of
          AvailableSession sess -> pure $ Just sess
          NoSession -> do
            writeTVar sessionVar GettingSession
            pure Nothing
          GettingSession -> retry

      case sessionM of
        Just session -> do
          pushLog "Session from cache"
          pure session
        Nothing -> do
          pushLog "Computing session from cache"
          logSection "getIguanaSession" $ do
            session <- getIguanaSession
            atomically $ writeTVar sessionVar (AvailableSession session)
            pure session

    auth <- logSection "getLogin" $ do
      auths <- readTVarIO authsVar
      case Map.lookup credential auths of
        Just auth -> do
          pushLog "Auth from cache"
          pure auth
        Nothing -> do
          logSection "getLoginFromServer" $ do
            auth <- getLogin session credential
            atomically $ modifyTVar authsVar (Map.insert credential auth)
            pure auth
    items <- logSection "getLoan" $ getLoan auth

    case eitherDecode @[JSONBook] (encode items) of
      Right books -> do
        mtime <- getCurrentTime
        pure $ Right (mtime, coerce books)
      Left err -> pure (Left $ Text.pack err)

renewBook :: (Text -> IO ()) -> (Book, User) -> IO (Either Text (UTCTime, [Book]))
renewBook pushLog (book, User {..}) = do
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
    res <- logSection "refreshLoan" $ refreshLoan auth book
    pushLog $ Text.pack $ show res
    -- TODO
    pure $ Left "toto"
