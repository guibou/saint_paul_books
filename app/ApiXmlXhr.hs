{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ApiXmlXhr where

import Api hiding (iguana_root)
import Books
import Control.Concurrent.Async (async)
import Control.Exception
import Control.Monad (void, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.ByteString.Char8 (toStrict)
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Debug.Trace (traceShow, traceShowId)
import ApiRequest
import Language.Javascript.JSaddle (MonadJSM)
import GHC.IO (unsafePerformIO)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Reflex.Dom.Core
import Data.Aeson.Types (Pair)
import Control.Lens ((<&>))
import Data.Functor (($>))
import Control.Monad.Fix (MonadFix)
import Data.Time.Calendar.OrdinalDate

-- TODO: here all the semantic is copy/pasted between the different action, and
-- the login phase is redone everytime, which maybe not optimal

refreshBook pushLog userEvent = do
  refreshBooks userEvent

renewBookAsync :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m)) => (Text -> IO ()) -> Event t (Book, User) -> m (Event t (Either Text Book))
renewBookAsync pushLog bookEvent = performEventAsync $ (renewBookCallback pushLog) <$> bookEvent

renewBookCallback :: (MonadIO m) => (Text -> IO ()) -> (Book, User) -> (Either Text Book -> IO a) -> m ()
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


{-# NOINLINE sessionVar #-}
sessionVar :: TVar SessionStatus
sessionVar = unsafePerformIO $ newTVarIO NoSession

authsVar :: TVar (Map Credential Auth)
authsVar = unsafePerformIO $ newTVarIO mempty

data SessionStatus = NoSession | GettingSession | AvailableSession IguanaSession

refreshBooks :: forall t m. (MonadWidget t m, MonadJSM (Performable m))
                       => Event t User
                       -> m (Event t (Either Text (UTCTime, [Book])))
refreshBooks eUser = do
  let eCredential = fmap credential eUser

  eMaybeSession <- getIguanaSession' eCredential
  let eSession = fmapMaybe id (traceEvent "session" eMaybeSession) -- On filtre le Just Session

  dCredential <- holdDyn (Credential{login="nologin", password="nopassword"}) eCredential
  dSession    <- holdDyn (IguanaSession "No session") eSession
  display dSession
  let eLoginTrigger = attachPromptlyDynWith (,) dCredential eSession
  let blork = Text.pack . show <$> eLoginTrigger
  eBlork <- holdDyn "NO blork" blork
  display eBlork

  eLoginTrigger2 <- delay 1 eLoginTrigger
  
  eAuth <- fmap switchDyn $ widgetHold (return never) $ fmap (\(cred, sess) -> do
      ePostBuild <- getPostBuild
      getLogin' sess cred ePostBuild
    ) eLoginTrigger

  -- On stocke l'Auth reçue dans une Dynamic
  dAuth <- holdDyn (Auth{iguanaSession = IguanaSession "no session auth yet", session = Session{sessionId = "no session id", token = "no useless token"}}) eAuth
  display dAuth

  eResult <- fmap switchDyn $ widgetHold (return never) $ fmap (\auth -> do
      getLoan' auth eAuth
    ) eAuth

  pure $ eResult <&> \items ->
    case eitherDecode @[JSONBook] (encode items) of
      Right books -> do
        Right (UTCTime (fromOrdinalDate 0 0) 0, coerce books)
      Left e -> Left (Text.pack e)

-- | Returns all the book owned by an authenticated user
getLoan' :: (Monad m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => Auth -> Event t x -> m (Event t [Data.Aeson.Value])
getLoan' Auth {..} triggerEvent = do
    responseEvent <- performRequestAsync (triggerEvent $> iguanaRequest' iguanaSession "user/loans"
      [ "sessionId" .= sessionId session,
        -- Note: most of these keys appears in the query I reverse
        -- engineered. I'm too lazy to try to remove some of them.
        "LocationProfile" .= ("" :: Text),
        "range" .= object ["from" .= (1 :: Int), "to" .= (10 :: Int)],
        "sort" .= object ["sortBy" .= ("!" :: Text), "sortDirection" .= ("ASC" :: Text)]
      ])
    pure $ fmap (\response -> do
                        case decodeXhrResponse response of
                          -- TODO: better error handling
                          Nothing -> []
                          Just (Response (Items items)) -> items
                        ) responseEvent

{-do
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
-}

renewBook :: (Text -> IO ()) -> (Book, User) -> IO (Either Text Book)
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
    case res of
      [RenewResponse{success = False, refusalReason}] -> pure (Left refusalReason)
      [RenewResponse{success = True, dueDate=newDueDate}] -> pure (Right (let Book{..} = book in Book{dueDate=newDueDate, ..}))
      _ -> pure (Left "Unknown error, see log")


-- | Log to the library. Returns an 'Auth' which is valid for a few time (not
-- clear how much).
getLogin' :: (Monad m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => IguanaSession -> Credential -> Event t x -> m (Event t Auth)
getLogin' iguanaSession Credential {..} triggerEvent = do
    res <- performRequestAsync (triggerEvent $> iguanaRequest' iguanaSession "user/credentials"
      -- Note: most of these keys appears in the query I reverse
      -- engineered. I'm too lazy to try to remove some of them.
      [ "language" .= ("fre" :: Text),
        "serviceProfile" .= ("Iguana" :: Text),
        "locationProfile" .= ("" :: Text),
        "user" .= login,
        "password" .= password,
        "institution" .= ("" :: Text)
      ])

    pure $ fmap (\response -> do
                            case decodeXhrResponse response of
                              Just (Response session) -> Auth{..}
                              Nothing -> Auth{session = Session{sessionId="error", token="wtf error"}, ..}
                            ) res


iguanaRequest' :: IguanaSession -> Text -> [Pair] -> XhrRequest _
iguanaRequest' IguanaSession {..} method payload = do
  let req = postJson (iguana_root <> "/Rest.Server.cls?sessionId=" <> iguanaSessionId <> "&method=" <> method) $ object ["request" .= object payload]
  req { _xhrRequest_config = (_xhrRequest_config req) { _xhrRequestConfig_withCredentials = True } }


getIguanaSession' :: (Monad m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => Event t b -> m (Event t (Maybe IguanaSession))
getIguanaSession' triggerEvent = do
  let request = XhrRequest "GET" (iguana_root <> "/Rest.Server.cls") (def {_xhrRequestConfig_withCredentials = True})
  response <- performRequestAsync (request <$ triggerEvent)
  pure $ extractIguanaSession <$> response

extractIguanaSession :: XhrResponse -> Maybe IguanaSession
extractIguanaSession response = do
  (traceShowId -> responseBody) <- _xhrResponse_responseText response
  let headers = _xhrResponse_headers response

  -- TODO: the extract session logic could be converted to Text
  let iguanaSessionId = extractSessionId (encodeUtf8 responseBody)

  pure $ traceShow headers $ IguanaSession {..}

-- | Represents the root of the iguana library
--
-- In the future this could be set as an API parameter so it may query other
-- iguana libraries
iguana_root :: Text
-- iguana_root = "https://mediatheques-saintpaul.re/iguana"
iguana_root = "https://spring-cake-0818.guillaum-bouchard.workers.dev/iguana"
