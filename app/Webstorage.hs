{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Webstorage (webStorageDyn) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.Javascript.JSaddle
import Reflex.Dom.Core

-- * Web Storage

-- | @@webStorageDyn name def evt@@ returns a 'Dynamic' which initial value is read from the webstorage field `name`, or from `def` and updated by 'evt'
webStorageDyn :: forall t a m. (_) => Text -> t -> Event a t -> m (Dynamic a t)
webStorageDyn name initVal evtUpdate = do
  initialValue <- webStorageInit name
  performEvent_ (saveWebStorage name <$> evtUpdate)
  holdDyn (fromMaybe initVal initialValue) evtUpdate

webStorageInit :: forall t m. (MonadJSM m, FromJSON t) => Text -> m (Maybe t)
webStorageInit name = liftJSM $ do
  jsVal <- jsg @Text "localStorage" >>= (! name)
  v <- fromJSVal @String jsVal

  case ((eitherDecode . LBS.pack) <$> v) of
    Just (Left _e) -> pure $ Nothing
    Just (Right v) -> pure v
    Nothing -> pure Nothing

saveWebStorage :: (ToJSON t, MonadJSM m, Show t) => Text -> t -> m ()
saveWebStorage name v = liftJSM $ do
  storage <- jsg @Text "localStorage"
  encodedVal <- toJSVal (LBS.unpack $ encode v)

  (storage <# name) encodedVal
