{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Api
import ApiXmlXhr (refreshBook, renewBookAsync)
import Books
import Control.Monad (join, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Char8 (ByteString)
import Data.FileEmbed
import Data.Functor (($>), (<&>))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as TE
import Data.Text.Read (decimal)
import Data.Time
import GHC.Generics
import Reflex.Dom
import Webstorage (webStorageDyn)
import Language.Javascript.JSaddle (liftJSM)
import Language.Javascript.JSaddle.Object
import Control.Lens ((^.), view, _2, _1, _4)

pattern CapacityPerUser :: Int
pattern CapacityPerUser = 10

pattern LoanMaxDays :: (Eq t, Num t) => t
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

data RefreshStatus = Refreshing | Idle | RefreshError Text
  deriving (Show, Eq)

isError :: _
isError (RefreshError _) = True
isError _ = False

css :: ByteString
css = $(embedFile "static/style.css")

nerdFontItem :: DomBuilder t m => Text -> m ()
nerdFontItem nfClass = elAttr "i" ("class" =: ("nf " <> nfClass)) $ pure ()

nerdFontButton :: (DomBuilder t m) => Text -> m (Event t ())
nerdFontButton nfClass = do
  (event, _) <- el' "span" $ nerdFontItem nfClass
  pure $ domEvent Click event

clickDiv innerWidget = do
  (event, _) <- el' "div" $ innerWidget
  pure $ domEvent Click event

data Visibility = BooksVisibility | SettingsVisibility | CardsVisibility
  deriving (Eq, Show)

pattern RefreshThreshold = 3600 -- auto refresh after one hour

logWithTime :: (Text -> IO ()) -> Text -> IO ()
logWithTime pushLog message = do
  time <- getCurrentTime
  pushLog (Text.pack (show time) <> message)

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "script" ("src" =: "https://cdn.jsdelivr.net/npm/jsbarcode@3.11.0/dist/JsBarcode.all.min.js") $ pure ()
  elAttr "style" ("type" =: "text/css") $ text $ TE.decodeUtf8 css

main :: IO ()
main = do
  mainWidgetWithHead headWidget $
    mdo
      pb <- getPostBuild
      now <- liftIO $ getCurrentTime
      tickE <- tickLossyFrom 1 now pb
      currentClockE <- performEvent (liftIO getCurrentTime <$ tickE)
      currentClock <- foldDyn (\now _ -> now) now currentClockE

      (eventLog, pushLog) <- newTriggerEvent
      webStorageSettings <- webStorageDyn "settings" defaultSettings (updated settingsDyn)
      initSetting <- sample $ current webStorageSettings
      credentialsUniqDyn <- holdUniqDyn ((credentials) <$> (settingsDyn))

      settingsDyn <- foldDyn (\f x -> f x) initSetting updateSettings

      let globalRefreshingStates = join $ fmap distributeListOverDyn ((map (view _1) . Map.elems) <$> allBooks')

      -- Header widget
      headerE <- el "div" $ do
        dyn $ flip fmap allBooks $ \books -> do
          elAttr "div" ("class" =: "header") $ do
            openMenuE <- nerdFontButton "nf-fa-bars"
            event <- elAttr "div" ("class" =: "menu") $ mdo

              menuVisibility <- foldDyn (\f v -> f v) False $ leftmost
                [(not <$ openMenuE),
                  (const False <$ refresh),
                  (const False <$ navigate)
                ]
              es@(refresh, navigate) <- elDynAttr "div" (menuVisibility <&> \visibility ->
                 if visibility then mempty else ("style" =: "display:none"))$ do
                booksE <- clickDiv $ taggedAsSelected changeVisibility BooksVisibility $ 
                  nerdFontButton "nf-cod-book" <* text "Livres"
                cardsE <- clickDiv $ taggedAsSelected changeVisibility CardsVisibility $ 
                  nerdFontButton "nf-fa-id_card" <* text "Cartes"
                refreshE <- clickDiv $ do
                  nerdFontButton "nf-md-reload" <* text "Reload"
                settingsE <- clickDiv $ taggedAsSelected changeVisibility SettingsVisibility $ 
                  nerdFontButton "nf-cod-settings_gear" <* text "Settings"
                pure
                  ( refreshE,
                    leftmost
                      [ BooksVisibility <$ booksE,
                        SettingsVisibility <$ settingsE,
                        CardsVisibility <$ cardsE
                      ]
                  )
              pure es

            case books of
              [] -> text "No user, add them in the Setting panel"; 
              _ -> do
                dyn_ $
                  globalRefreshingStates <&> \refreshingState ->
                    if
                      | all (== Idle) refreshingState -> do
                          let oldest_update = minimum (map fst books)

                          let total = sum $ map (\(_mtime, items) -> length @[] items) books
                              capacity = length books * CapacityPerUser

                          text "Capacity: "
                          text $ tshow total <> "/" <> tshow capacity
                          text " Refreshed: "

                          let niceAgeDyn = fromUniqDynamic $ uniqDynamic $ (currentClock <&> \now -> do
                                let iguanaAge = now `diffUTCTime` oldest_update
                                niceAge iguanaAge
                                )

                          dynText niceAgeDyn

                      | any isError refreshingState -> text $ "Error when refreshing, see logs"
                      | otherwise -> do
                          elAttr
                            "progress"
                            []
                            $ text
                              ""
                          let nb_done = length $ filter (== Idle) refreshingState
                          text "Refreshing"
                          text $ tshow $ nb_done
                          text "/"
                          text $ tshow $ length refreshingState
            pure event
      refreshnerdFontButtonE <- switchHold never (fst <$> headerE)
      changeVisibilityE <- switchHold never (snd <$> headerE)
      changeVisibility <- foldDyn const BooksVisibility changeVisibilityE

      let today = fromUniqDynamic $ uniqDynamic $ utctDay <$> currentClock

      -- Listing widget
      let allBooks = join $ fmap distributeListOverDyn $ (map (view _2) . Map.elems) <$> allBooks'
      let chevalBooks = join $ fmap distributeListOverDyn $ (fmap (\(a, b, c) -> (,,) <$> a <*> b <*> c) . Map.elems) <$> allBooks'
      let chevalBooks1 = concatMap (\(rs, (t, books), user) -> (rs, t, user, ) <$> books) <$> chevalBooks

      let visibleDiv = elDivVisible changeVisibility

      visibleDiv BooksVisibility $ do
        elAttr "table" ("class" =: "books") $ do
          _ <- listWithKey ((Map.fromList . zip [0 :: Int ..] . sortOn (dueDate . view _4)) <$> chevalBooks1) $ \_idx d -> do
            dyn_ ((\(_, _, user, book) -> displayBook pushLog user book today) <$> d)
            pure ()
          pure ()

      allBooks' <- visibleDiv CardsVisibility $ listWithKey (fmap (Map.fromList . map (\u -> (login (credential u), u))) credentialsUniqDyn) $ \login userDyn -> mdo
        webStorageBooks <- webStorageDyn ("book" <> login) (now, []) (updated booksDyn)
        initBooks <- sample $ current webStorageBooks
        booksDyn <- foldDyn (\new _old -> new) initBooks updateBooks

        let latestRefresh = fst <$> booksDyn
        let shouldAutoRefresh = attachWith (\(latestRefresth, status) now ->
                         now `diffUTCTime` latestRefresth > RefreshThreshold && status /= Refreshing)
                           ((,) <$> current latestRefresh <*> current refreshStatus) (updated currentClock)
        let autoRefreshEvent = ffilter (\v -> v) $ shouldAutoRefresh

        let refreshEvent =
              leftmost
                [ updated userDyn,
                  tag (current userDyn) refreshnerdFontButtonE,
                  tag (current userDyn) autoRefreshEvent
                ]
        (fanEither -> (updateError, updateBooks)) <- refreshBook (logWithTime pushLog) refreshEvent

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
                  RefreshError t -> text t
          el "div" $ do
             -- I don't really understand, but if there is no delay, the
             -- barcode is not displayed correctly.
             -- I will investigate later, but for now, it works simply.
             b <- getPostBuild
             b' <- delay 1 b
             barcodeWidget b' login

          booksDyn <- elAttr "table" ("class" =: "books") $ do
            -- TODO: the complete block is rebuilt if anything changes, but
            -- that's fine. Maybe later we could introduce finer grained
            -- updates, for example, based on barcode
            void $ simpleList (snd <$> booksDyn) $ \bookDyn ->
              dyn $
                ((,) <$> bookDyn <*> userDyn) <&> \(book, user) ->
                  displayBook pushLog user book today
            pure $ booksDyn

          pure (refreshStatus, booksDyn, userDyn)

      -- Settings
      --
      updateSettings <- visibleDiv SettingsVisibility $ do
        updateSettings <- settingsPanel settingsDyn
        el "details" $ do
          el "summary" $ text "logs"
          -- Keep only the latest 100 lines of log
          logDyn <- foldDyn (\x l -> take 100 $ x : l) [] eventLog
          el "pre" $ dynText (Text.unlines <$> logDyn)
        pure updateSettings
      pure ()

taggedAsSelected currentVisibilty targetedVisibility widget = elDynAttr "div" (currentVisibilty <&> \v -> if v == targetedVisibility then "class" =: "current" else mempty) $ widget

elDivVisible :: (DomBuilder t m, PostBuild t m) => Dynamic t Visibility -> Visibility -> m a -> m a
elDivVisible currentVisibleDyn name content = do
  let toVisibility :: Visibility -> Map.Map Text Text
      toVisibility current
        | current == name = ("style" =: "display: block")
        | otherwise = ("style" =: "display: none")

  elDynAttr "div" (toVisibility <$> currentVisibleDyn) content

displayBook :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m)) => (Text -> IO ()) -> User -> Book -> Dynamic t Day -> m ()
displayBook pushLog user book today = do
  el "tr" $ do
    let remainingDays = diffDays (dueDate book) <$> today
    let colorClass remainingDays
          | remainingDays > LoanMaxDays `div` 2 = "" :: Text
          | remainingDays > 3 = "late-ok"
          | otherwise = "late-critical"
    el "td" $ mdo
      showE <- nerdFontButton "nf-fa-image"

      visibleDyn <- foldDyn (\f x -> f x) False $ leftmost [not <$ showE, const False <$ closeE]

      closeE <- elDynAttr "el" (visibleDyn <&> \visible -> "class" =: "details" <> "style" =: if visible then "display: block" else "display: none") $ do
        closeE <- elAttr "div" ("class" =: "close") $ nerdFontButton "nf-md-close_circle"
        elAttr "img" ("src" =: cover book) $ pure ()
        el "div" $ text $ "author:" <> author book
        el "div" $ text $ "full title:" <> fullTitle book

        renewE <- button "Renew"
        renewBookAsync pushLog (renewE $> (book, user))

        pure $ closeE
      pure ()
    el "td" $ text $ Text.take 1 (displayName user)
    elDynAttr "td" (("class" =:) . colorClass <$> remainingDays) $ dynText $ (( \remainingDays -> tshow remainingDays <> "d") <$> remainingDays)
    el "td" $ mdo
      (e, _) <- elDynAttr' "div" (set <&> \b -> if b then Map.singleton "style" "text-decoration-line: line-through" else []) $ text $ title book
      let click = domEvent Click e
      set <- foldDyn (\_ v -> not v) False click
      pure ()
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

-- | Display a barcode code128 on a event
barcodeWidget :: _ => _ -> _ -> m ()
barcodeWidget event value = do
            elAttr "canvas" (
              "class" =: "barcode" <>
              "jsbarcode-format" =: "code128" <>
              "jsbarcode-value" =: value <>
              "jsbarcode-textmargin" =: "0" <>
              "jsbarcode-fontoptions" =: "bold" <>
              "jsbarcode-height" =: "40"
               ) $ pure ()

            let
                foo = liftJSM $ do
                   v <- jsg1 ("JsBarcode" :: String) (".barcode" :: String)
                   _ <- v ^. js0 ("init" :: String)

                   pure ()
            performEvent_ (event $> foo)
