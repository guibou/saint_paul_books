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
import Control.Concurrent.Async (async)
import Control.Exception
import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Functor (($>), (<&>))
import qualified Data.Map.Strict as Map
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

refreshBooks :: User -> IO (Either Text (UTCTime, [Book]))
refreshBooks User {..} = do
  resM <- try $ do
    auth <- getLogin credential
    items <- getLoan auth
    -- TODO: handle error here
    pure items

  -- Write the payload to the disk
  -- encodeFile ".iguana.json" res

  case resM of
    Right res -> do
      case eitherDecode @[Book] (encode res) of
        Right books -> do
          mtime <- getCurrentTime
          pure $ Right (mtime, books)
        Left err -> pure (Left $ Text.pack err)
    Left (err :: SomeException) -> do
      pure $ Left (tshow err)

data RefreshStatus = Refreshing | Idle | RefreshError Text
  deriving (Show)

main :: IO ()
main = do
  mainWidget $
    mdo
      webStorageSettings <- webStorageDyn "settings" defaultSettings (updated settingsDyn)
      initSetting <- sample $ current webStorageSettings
      credentialsUniqDyn <- holdUniqDyn ((credentials) <$> (settingsDyn))

      settingsDyn <- foldDyn (\f x -> f x) initSetting updateSettings

      -- Header widget
      refreshButtonE <- el "div" $ do
        dyn $ flip fmap allBooks $ \books -> do
          case books of
            [] -> text "No user, add them in the Setting panel"
            _ -> do
              let oldest_update = minimum (map fst books)
              now <- liftIO $ getCurrentTime
              let iguanaAge = now `diffUTCTime` oldest_update

              let total = sum $ map (\(_mtime, items) -> length @[] items) books
                  capacity = length books * CapacityPerUser

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
                $ text
                  ""
              text $
                niceAge iguanaAge
          button "⟳"

      refreshButton <- switchHold never refreshButtonE

      now <- liftIO $ getCurrentTime
      let today = utctDay now

      -- Listing widget
      let allBooks = join $ fmap distributeListOverDyn $ Map.elems <$> allBooks'
      allBooks' <- listWithKey (fmap (Map.fromList . map (\u -> (login (credential u), u))) credentialsUniqDyn) $ \login userDyn -> mdo
        webStorageBooks <- webStorageDyn ("book" <> login) (now, []) (updated booksDyn)
        initBooks <- sample $ current webStorageBooks
        booksDyn <- foldDyn (\new _old -> new) initBooks updateBooks

        let refreshEvent =
              leftmost
                [ updated userDyn,
                  tag (current userDyn) refreshButton
                ]
        (fanEither -> (updateError, updateBooks)) <-
          performEventAsync $
            refreshBooksCallback
              <$> refreshEvent

        refreshStatus <-
          foldDyn (\new _old -> new) Idle $
            leftmost
              [ updateBooks
                  $> Idle,
                RefreshError
                  <$> updateError,
                refreshEvent
                  $> Refreshing
              ]

        el "details" $ do
          el "summary" $ do
            dynText (displayName <$> userDyn)
            text " "

            dyn_ $
              refreshStatus <&> \status -> do
                case status of
                  Refreshing -> do
                    el "progress" $ text ""
                  Idle -> do
                    dyn_ $
                      booksDyn <&> \(_, books) -> do
                        text $ tshow $ length books
                        text $ " / " <> tshow CapacityPerUser
                        text " "
                        let minRemainingDays = case books of
                              [] -> LoanMaxDays
                              _ -> minimum $ map (\book -> fromIntegral $ diffDays (dueDate book) today) books
                        elDynAttr
                          "meter"
                          ( settingsDyn
                              <&> \Settings {..} ->
                                [ ("min", "0"),
                                  ("max", tshow LoanMaxDays),
                                  ("low", tshow loanDayThreshold),
                                  ("value", tshow minRemainingDays)
                                ]
                          )
                          $ text ""
                  RefreshError t -> text t
          el "table" $ do
            -- TODO: the complete block is rebuilt if anything changes, but
            -- that's fine. Maybe later we could introduce finer grained updates
            void $ simpleList (snd <$> booksDyn) $ \bookDyn ->
              dyn $
                bookDyn <&> \book ->
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
            pure $ booksDyn

      -- Settings
      --
      updateSettings <- el "details" $ do
        el "summary" $ text "Settings"
        updateSettings <- settingsPanel settingsDyn
        pure updateSettings
      pure ()

refreshBooksCallback :: (MonadIO m) => User -> (Either Text (UTCTime, [Book]) -> IO ()) -> m ()
refreshBooksCallback credential callback = do
  void $ liftIO $ async $ do
    resM <- refreshBooks credential
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
