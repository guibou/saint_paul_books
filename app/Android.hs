{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Coerce (coerce)
import Data.Functor (($>), (<&>))
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
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

tread :: Text -> Maybe Int
tread t = case decimal t of
  Right (i, "") -> Just i
  _ -> Nothing

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
      pure $ Left (tshow err)

data RefreshStatus = Refreshing | Idle | RefreshError Text
  deriving (Show, Eq)

isError :: _
isError (RefreshError _) = True
isError _ = False

css :: ByteString
css =
  pack $
    intercalate
      "\n"
      [ "",
        ".details {",
        "   position: absolute;",
        "   top: 0px;",
        "   left: 0px;",
        "   background: white;",
        "}",
        ""
      ]

data Visibility = BooksVisibility | SettingsVisibility | CardsVisibility
  deriving (Eq, Show)

main :: IO ()
main = do
  mainWidgetWithCss css $
    mdo
      webStorageSettings <- webStorageDyn "settings" defaultSettings (updated settingsDyn)
      initSetting <- sample $ current webStorageSettings
      credentialsUniqDyn <- holdUniqDyn ((credentials) <$> (settingsDyn))

      settingsDyn <- foldDyn (\f x -> f x) initSetting updateSettings

      let globalRefreshingStates = join $ fmap distributeListOverDyn ((map fst . Map.elems) <$> allBooks')

      -- Header widget
      headerE <- el "div" $ do
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

              dyn_ $
                globalRefreshingStates <&> \refreshingState ->
                  if
                    | all (== Idle) refreshingState -> text $ niceAge iguanaAge
                    | any isError refreshingState -> text $ "Error"
                    | otherwise -> do
                        let nb_done = length $ filter (== Idle) refreshingState
                        text "Refreshing"
                        text $ tshow $ nb_done
                        text "/"
                        text $ tshow $ length refreshingState
          booksE <- elAttr "a" ("href" =: "#books") $ button "🕮"
          cardsE <- elAttr "a" ("href" =: "#cards") $ button "🃟"
          text "-"
          refreshE <- button "⟳"
          text "-"
          settingsE <- elAttr "a" ("href" =: "#settings") $ button "⚙"
          pure
            ( refreshE,
              leftmost
                [ BooksVisibility <$ booksE,
                  SettingsVisibility <$ settingsE,
                  CardsVisibility <$ cardsE
                ]
            )

      refreshButtonE <- switchHold never (fst <$> headerE)
      changeVisibilityE <- switchHold never (snd <$> headerE)
      changeVisibility <- foldDyn const BooksVisibility changeVisibilityE

      now <- liftIO $ getCurrentTime
      let today = utctDay now

      -- Listing widget
      let allBooks = join $ fmap distributeListOverDyn $ (map snd . Map.elems) <$> allBooks'
      let allBooksInOneList = fmap (concat . fmap snd) allBooks

      let visibleDiv = elDivVisible changeVisibility

      visibleDiv BooksVisibility $ do
        _ <- listWithKey ((Map.fromList . zip [0 :: Int ..] . sortOn dueDate) <$> allBooksInOneList) $ \_idx bookDyn -> do
          dyn_ ((\book -> displayBook book today settingsDyn) <$> bookDyn)
          pure ()
        pure ()

      allBooks' <- visibleDiv CardsVisibility $ listWithKey (fmap (Map.fromList . map (\u -> (login (credential u), u))) credentialsUniqDyn) $ \login userDyn -> mdo
        webStorageBooks <- webStorageDyn ("book" <> login) (now, []) (updated booksDyn)
        initBooks <- sample $ current webStorageBooks
        booksDyn <- foldDyn (\new _old -> new) initBooks updateBooks

        let refreshEvent =
              leftmost
                [ updated userDyn,
                  tag (current userDyn) refreshButtonE
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
          booksDyn <- el "table" $ do
            -- TODO: the complete block is rebuilt if anything changes, but
            -- that's fine. Maybe later we could introduce finer grained updates
            void $ simpleList (snd <$> booksDyn) $ \bookDyn ->
              dyn $
                bookDyn <&> \book ->
                  displayBook book today settingsDyn
            pure $ booksDyn

          pure (refreshStatus, booksDyn)

      -- Settings
      --
      updateSettings <- visibleDiv SettingsVisibility $ do
        updateSettings <- settingsPanel settingsDyn
        pure updateSettings
      pure ()

elDivVisible :: (DomBuilder t m, PostBuild t m) => Dynamic t Visibility -> Visibility -> m a -> m a
elDivVisible currentVisibleDyn name content = do
  let toVisibility :: Visibility -> Map.Map Text Text
      toVisibility current
        | current == name = ("style" =: "display: block")
        | otherwise = ("style" =: "display: none")

  elDynAttr "div" (toVisibility <$> currentVisibleDyn) content

displayBook :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m) => Book -> Day -> Dynamic t Settings -> m ()
displayBook book today settingsDyn = do
  el "tr" $ do
    let remainingDays = diffDays (dueDate book) today
    let elapsedDays = LoanMaxDays - fromIntegral remainingDays
    el "td" $ mdo
      (e, _) <- elDynAttr' "div" (set <&> \b -> if b then Map.singleton "style" "text-decoration-line: line-through" else []) $ text $ title book
      let click = domEvent Click e
      set <- foldDyn (\_ v -> not v) False click
      pure ()
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
    el "td" $ mdo
      showE <- button "🛈"

      visibleDyn <- foldDyn (\f x -> f x) False $ leftmost [not <$ showE, const False <$ closeE]

      closeE <- elDynAttr "el" (visibleDyn <&> \visible -> "class" =: "details" <> "style" =: if visible then "display: block" else "display: none") $ do
        closeE <- elAttr "div" ("class" =: "close") $ button "x"
        elAttr "img" ("src" =: cover book) $ pure ()
        el "div" $ text $ "author:" <> author book
        el "div" $ text $ "full title:" <> fullTitle book
        pure $ closeE
      pure ()

refreshBooksCallback :: (MonadIO m) => User -> (Either Text (UTCTime, [Book]) -> IO ()) -> m ()
refreshBooksCallback credential callback = do
  void $ liftIO $ async $ do
    resM <- refreshBooks credential
    callback resM
  pure ()

settingsPanel :: (DomBuilder t m, MonadSample t m, PostBuild t m, MonadFix m, MonadHold t m) => Dynamic t Settings -> m (Event t (Settings -> Settings))
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

inputNumber :: (DomBuilder t m, MonadHold t m, MonadFix m) => Int -> m (Dynamic t Int)
inputNumber defaultValue = mdo
  elem <-
    inputElement
      ( def
          & inputElementConfig_initialValue
          .~ tshow defaultValue
          & inputElementConfig_elementConfig
          . elementConfig_initialAttributes
          .~ [("type", "number")]
          & inputElementConfig_elementConfig
          . elementConfig_modifyAttributes
          .~ (e <&> \v -> [("type", Just "number"), ("style", if v then Nothing else Just "border-color: red")])
      )

  let e = (updated $ value elem) <&> \v -> isJust (tread v)

  foldDyn
    ( \t old -> case tread t of
        Nothing -> old
        Just new -> new
    )
    defaultValue
    (updated $ value elem)
