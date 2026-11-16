{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module ApiXmlXhr where

import Api
import Books
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Coerce (coerce)
import Data.Functor (($>), (<&>))
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import GHC.IO (unsafePerformIO)
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom.Core
import ApiRequest
import Control.Monad.Fix (MonadFix)

-- TODOS
-- - restore the logs
-- - restore the "caching" of the sessionid/credential steps
-- - Cleanup the mess generated during the transition from IO to reflex XHR

refreshBook :: (MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m,  Adjustable t m, PostBuild t m, MonadHold t m) => p -> Event t User -> m (Event t (Either Text (UTCTime, [Book])))
refreshBook pushLog userEvent = do
  refreshBooks userEvent

{-# NOINLINE sessionVar #-}
sessionVar :: TVar SessionStatus
sessionVar = unsafePerformIO $ newTVarIO NoSession

-- TODO: funny, I forgot the NOINLINE here, it may be the reason why so much
-- issue happened with authentifications.
authsVar :: TVar (Map Credential Auth)
authsVar = unsafePerformIO $ newTVarIO mempty

data SessionStatus = NoSession | GettingSession | AvailableSession IguanaSession

getAuth :: (MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m,  Adjustable t m, PostBuild t m, MonadHold t m) => Event t User -> m (Event t Auth)
getAuth eUser = do
  let eCredential = fmap credential eUser

  eMaybeSession <- getIguanaSession' eCredential
  let eSession = fmapMaybe id (eMaybeSession) -- On filtre le Just Session
  dCredential <- holdDyn (Credential {login = "nologin", password = "nopassword"}) eCredential
  let eLoginTrigger = attachPromptlyDynWith (,) dCredential eSession

  eAuth <-
    fmap switchDyn $
      widgetHold (return never) $
        fmap
          ( \(cred, sess) -> do
              ePostBuild <- getPostBuild
              getLogin' sess cred ePostBuild
          )
          eLoginTrigger
  pure eAuth

refreshBooks ::
  forall t m.
  (MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m,  Adjustable t m, PostBuild t m, MonadHold t m) =>
  Event t User ->
  m (Event t (Either Text (UTCTime, [Book])))
refreshBooks eUser = do
  eAuth <- getAuth eUser

  eResult <-
    fmap switchDyn $
      widgetHold (return never) $
        fmap
          ( \auth -> do
              ePostBuild <- getPostBuild
              getLoan' auth ePostBuild
          )
          eAuth

  eReservation <-
    fmap switchDyn $
      widgetHold (return never) $
        fmap
          ( \auth -> do
              ePostBuild <- getPostBuild
              getReservation auth ePostBuild
          )
          eAuth

  d1 <- holdDyn [] eResult
  d2 <- holdDyn [] eReservation
  let dCombined = zipDynWith (\xs ys -> (xs <> ys)) d1 d2
  let res = updated dCombined

  eResultWithTime <- performEvent (res <&> \items -> do
     curTime <- liftIO getCurrentTime
     pure (curTime, items)
     )


  pure $
    eResultWithTime <&> \(refreshTime, items) ->
      case eitherDecode @[JSONBook] (encode items) of
        Right books -> do
          Right (refreshTime, coerce books)
        Left e -> Left (Text.pack e)

-- | Returns all the book owned by an authenticated user
getLoan' :: (Monad m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => Auth -> Event t x -> m (Event t [Data.Aeson.Value])
getLoan' Auth {..} triggerEvent = do
  responseEvent <-
    performRequestAsync
      ( triggerEvent
          $> iguanaRequest'
            iguanaSession
            "user/loans"
            [ "sessionId" .= sessionId session,
              -- Note: most of these keys appears in the query I reverse
              -- engineered. I'm too lazy to try to remove some of them.
              "LocationProfile" .= ("" :: Text),
              "range" .= object ["from" .= (1 :: Int), "to" .= (10 :: Int)],
              "sort" .= object ["sortBy" .= ("!" :: Text), "sortDirection" .= ("ASC" :: Text)]
            ]
      )
  pure $
    fmap
      ( \response -> do
          case decodeXhrResponse $ response of
            -- TODO: better error handling
            Nothing -> []
            Just (Response (Items items)) -> items
      )
      responseEvent

getReservation :: (MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => Auth -> Event t a -> m (Event t [Data.Aeson.Value])
getReservation Auth {..} triggerEvent = do
  responseEvent <-
    performRequestAsync
      ( triggerEvent
          $> iguanaRequest'
            iguanaSession
            "user/reservations"
            [ "sessionId" .= sessionId session,
              -- Note: most of these keys appears in the query I reverse
              -- engineered. I'm too lazy to try to remove some of them.
              "LocationProfile" .= ("" :: Text),
              "range" .= object ["from" .= (1 :: Int), "to" .= (10 :: Int)],
              "sort" .= object ["sortBy" .= ("!" :: Text), "sortDirection" .= ("ASC" :: Text)]
            ]
      )
  pure $
    fmap
      ( \response -> do
          case decodeXhrResponse $ response of
            -- TODO: better error handling
            Nothing -> []
            Just (Response (Items items)) -> items
      )
      responseEvent

renewBook ::
  (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m), MonadJSM (Performable m), Adjustable t m, PostBuild t m, MonadHold t m, DomBuilder t m, MonadFix m, MonadSample t (Performable m)) =>
    -- | Log mecanism
    (Text -> IO ())
    -- | An event with current book and user I want to renew
    -> Event t (Book, User)
    -- | Output book
    -> m (Event t (Either Text Book))
renewBook pushLog eBookUser = do
  dBookUser <- holdDyn (error "not yet") eBookUser
  eAuth <- getAuth (snd <$> eBookUser)

  eResult <-
    fmap switchDyn $
      widgetHold (return never) $
        fmap
          ( \auth -> do
              ePostBuild <- getPostBuild
              bookUser <- sample (current dBookUser)
              let book = fst bookUser
              fmap (book,) <$> refreshLoan auth book ePostBuild
          )
          eAuth
  pure $ eResult <&> \(book, res) -> case res of
       [RenewResponse {success = False, refusalReason}] -> Left refusalReason
       [RenewResponse {success = True, dueDate = newDueDate}] -> Right (let Book {..} = book in Book {status = DueAt newDueDate, ..})
       _ -> (Left "Unknown error, see log")

refreshLoan :: 


  (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m), MonadJSM (Performable m), Adjustable t m, PostBuild t m, MonadHold t m) => Auth -> Book -> Event t a -> (m (Event t [RenewResponse]))
refreshLoan Auth {..} Book {..} triggerEvent = do
  responseEvent <-
    performRequestAsync
      ( triggerEvent $>
    iguanaRequest' iguanaSession "user/renewal" 
      [ "sessionId" .= sessionId session,
        "items" .= barcode
      ])

  pure $
    fmap
      ( \response -> do
          case decodeXhrResponse $ response of
            -- TODO: better error handling
            Nothing -> []
            Just (Response (Results val)) -> val
      )
      responseEvent

-- | Log to the library. Returns an 'Auth' which is valid for a few time (not
-- clear how much).
getLogin' :: (Monad m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => IguanaSession -> Credential -> Event t x -> m (Event t Auth)
getLogin' iguanaSession Credential {..} triggerEvent = do
  res <-
    performRequestAsync
      ( triggerEvent
          $> iguanaRequest'
            iguanaSession
            "user/credentials"
            -- Note: most of these keys appears in the query I reverse
            -- engineered. I'm too lazy to try to remove some of them.
            [ "language" .= ("fre" :: Text),
              "serviceProfile" .= ("Iguana" :: Text),
              "locationProfile" .= ("" :: Text),
              "user" .= login,
              "password" .= password,
              "institution" .= ("" :: Text)
            ]
      )

  pure $
    fmap
      ( \response -> do
          case decodeXhrResponse response of
            Just (Response session) -> Auth {..}
            Nothing -> Auth {session = Session {sessionId = "error", token = "wtf error"}, ..}
      )
      res

iguanaRequest' :: IguanaSession -> Text -> [Pair] -> XhrRequest Text
iguanaRequest' IguanaSession {..} method payload = do
        postJson iguana_root $
          object ["payload" .= object ["request" .= object payload],
                  "sessionId" .= iguanaSessionId,
                  "cookies" .= cookies,
                  "method" .= method
                 ]

getIguanaSession' :: (Monad m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => Event t b -> m (Event t (Maybe IguanaSession))
getIguanaSession' triggerEvent = do
  let request = XhrRequest "GET" iguana_root def
  response <- performRequestAsync (request <$ triggerEvent)
  pure $ extractIguanaSession <$> response

extractIguanaSession :: XhrResponse -> Maybe IguanaSession
extractIguanaSession response = do
  decodeXhrResponse $ response

-- | Represents the root of the iguana library, in a cloudfare proxy
--
-- In the future this could be set as an API parameter so it may query other
-- iguana libraries
iguana_root :: Text
iguana_root = "https://spring-cake-0818.guillaum-bouchard.workers.dev/"
