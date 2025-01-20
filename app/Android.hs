{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Api
import Books
import Control.Concurrent.Async (async, forConcurrently)
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time
import GHC.Generics
import Reflex.Dom
import Webstorage (webStorageDyn)

pattern CapacityPerUser :: Int
pattern CapacityPerUser = 10

pattern LoanMaxDays :: Int
pattern LoanMaxDays = 28

pattern DefaultCapacityThreshold :: Int
pattern DefaultCapacityThreshold = 8

pattern DefaultLowRemainingDays :: Int
pattern DefaultLowRemainingDays = 5

tshow :: (Show t) => t -> Text
tshow = Text.pack . show

tread :: (Read t) => Text -> t
tread = read . Text.unpack

data Settings = Settings
  { capacityThreshold :: Int,
    loanDayThreshold :: Int,
    credentials :: [User]
  }
  deriving (Show, FromJSON, ToJSON, Generic)

defaultSettings :: Settings
defaultSettings =
  Settings
    { capacityThreshold = 8,
      loanDayThreshold = 5,
      credentials = []
    }

niceAge :: NominalDiffTime -> Text
niceAge (truncate -> age)
  | (age :: Int) < 60 = tshow age <> " secondes ago"
  | age < 3600 = tshow (age `div` 60) <> " minutes ago"
  | age < (3600 * 24) = tshow (age `div` 3600) <> " hours ago"
  | otherwise = tshow (age `div` (3600 * 24)) <> " days ago"

refreshAction :: [User] -> IO (Either String (UTCTime, [(Text, [Book])]))
refreshAction users = do
  resM <- try $ forConcurrently users $ \User {..} -> do
    auth <- login (Credential {..})
    items <- getLoan auth
    -- TODO: handle error here
    pure (name, items)

  -- Write the payload to the disk
  -- encodeFile ".iguana.json" res

  case resM of
    Right res -> do
      let (fromMaybe [] -> peoples) = decode @[(Text, [Book])] (encode res)
      mtime <- getCurrentTime
      pure $ Right (mtime, peoples)
    Left (err :: SomeException) -> do
      pure $ Left (show err)

main :: IO ()
main = do
  mtime <- getCurrentTime

  mainWidget $
    mdo
      webStorageSettings <- webStorageDyn "settings" defaultSettings (updated settingsDyn)
      initSetting <- sample $ current webStorageSettings

      settingsDyn <- foldDyn (\f x -> f x) initSetting updateSettings

      credentialsUniqDyn <- holdUniqDyn ((credentials) <$> (settingsDyn))
      let refreshEvent =
            leftmost
              [ updated credentialsUniqDyn,
                tag (current credentialsUniqDyn) refreshButton
              ]

      refreshEventWithNewItems <-
        performEventAsync
          ( fmap eventCallback refreshEvent
          )

      webStoragePeoples <- webStorageDyn "peoples" (mtime, []) (updated peoplesDyn)
      initPeoples <- sample $ current webStoragePeoples

      totoDyn <-
        foldDyn
          ( \x'M (current, _errorMsg) -> do
              case x'M of
                Left errMsg -> do
                  (current, Text.pack errMsg)
                Right x' -> (x', "")
          )
          (initPeoples, "")
          refreshEventWithNewItems
      let peoplesDyn = fst <$> totoDyn
      let updateDebug = updated (snd <$> totoDyn)

      -- Header widget
      refreshButton <- el "div" $ do
        dyn_ $ flip fmap peoplesDyn $ \(mtime, peoples) -> do
          now <- liftIO $ getCurrentTime
          let iguanaAge = now `diffUTCTime` mtime

          let total = sum $ map (\(_name, items) -> length items) peoples
              capacity = length peoples * CapacityPerUser
          text $ tshow total <> "/" <> tshow capacity
          elDynAttr
            "meter"
            ( settingsDyn
                <&> \Settings {..} ->
                  [ ("min", "0"),
                    ("max", tshow capacity),
                    ("high", tshow $ capacity - capacityThreshold),
                    ("value", tshow total)
                  ]
            )
            $ text ""
          text $ niceAge iguanaAge
        button "⟳"

      -- Listing widget
      dyn_ $ flip fmap peoplesDyn $ \(_, peoples) -> do
        now <- liftIO $ getCurrentTime
        let today = utctDay now

        for_ peoples $ \(name, books) -> do
          el "details" $ do
            el "summary" $ text $ name <> " " <> tshow (length books) <> " / " <> tshow CapacityPerUser
            el "table" $ do
              for_ books $ \book -> do
                el "tr" $ do
                  let remainingDays = diffDays (dueDate book) today
                  let elapsedDays = LoanMaxDays - fromIntegral remainingDays
                  el "td" $ text $ title book
                  el "td" $ text $ tshow remainingDays <> " days"
                  el "td"
                    $ elDynAttr
                      "meter"
                      ( settingsDyn
                          <&> \Settings {..} ->
                            [ ("min", "0"),
                              ("max", tshow LoanMaxDays),
                              ("low", tshow loanDayThreshold),
                              ("value", tshow remainingDays)
                            ]
                      )
                    $ text
                    $ tshow elapsedDays <> " / " <> tshow LoanMaxDays

      -- Settings
      --
      updateSettings <- el "details" $ do
        el "summary" $ text "Settings"
        updateSettings <- settingsPanel settingsDyn
        _ <- el "details" $ do
          -- debug
          el "summary" $ text "debug"
          do
            textAreaElement
              ( def
                  & textAreaElementConfig_initialValue
                  .~ "Everything is fine"
                  & ( textAreaElementConfig_setValue
                        .~ updateDebug
                    )
              )
        pure updateSettings
      pure ()

eventCallback :: (MonadIO m) => [User] -> (Either String (UTCTime, [(Text, [Book])]) -> IO ()) -> m ()
eventCallback credentials callback = do
  void $ liftIO $ async $ do
    resM <- refreshAction credentials
    callback resM
  pure ()

settingsPanel :: (DomBuilder t m, MonadSample t m, PostBuild t m) => Dynamic t Settings -> m (Event t (Settings -> Settings))
settingsPanel settingsDyn = do
  el "table" $ do
    Settings {..} <- sample $ current settingsDyn
    capacityThresholdDyn <- el "tr" $ do
      el "td" $ text "Total capacity threshold"
      el "td" $ do
        inputNumber capacityThreshold

    loanDayThresholdDyn <- el "tr" $ do
      el "td" $ text "Loan day threshold"
      el "td" $ inputNumber loanDayThreshold

    credentialsDyn <- el "details" $ do
      el "summary" $ text "Credentials"
      credValue <-
        value
          <$> textAreaElement
            ( def
                & textAreaElementConfig_initialValue
                .~ (toStrict $ decodeUtf8 $ encodePretty credentials)
                & textAreaElementConfig_elementConfig
                . elementConfig_initialAttributes
                .~ []
            )
      el "div" $
        dyn_ $
          credValue <&> \t -> case eitherDecodeStrict @[User] (encodeUtf8 t) of
            Right _ -> text ""
            Left e -> text $ "Error: " <> Text.pack e
      pure credValue

    pure $
      leftmost
        [ (\v -> \settings -> settings {capacityThreshold = v}) <$> updated capacityThresholdDyn,
          ((\v settings -> settings {loanDayThreshold = v}) <$> updated loanDayThresholdDyn),
          ( ( \v settings -> case decodeStrict (encodeUtf8 v) of
                Nothing -> settings
                Just credentials' -> settings {credentials = credentials'}
            )
              <$> updated credentialsDyn
          )
        ]

inputNumber :: (DomBuilder t m) => Int -> m (Dynamic t Int)
inputNumber defaultValue = do
  elem <-
    inputElement
      ( def
          & inputElementConfig_initialValue
          .~ tshow defaultValue
          & inputElementConfig_elementConfig
          . elementConfig_initialAttributes
          .~ [("type", "number")]
      )
  pure $ fmap tread (value elem)
