{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS -Wall #-}

import Control.Monad (forM, forM_)
import Data.Aeson
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time
import GHC.Generics (Generic)
import PyF
import Refresh (User (..))

data Payload = Payload {response :: Response}
  deriving (Show, FromJSON, Generic)

data Response = Response {items :: [Item]}
  deriving (Show, FromJSON, Generic)

data Item = Item {title :: Text, dueDate :: Day}
  deriving (Show, Generic)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> do
    Item <$> (cleanTitle <$> o .: "title") <*> (cleanDate =<< o .: "dueDate")

cleanDate :: (MonadFail m) => String -> m Day
cleanDate = parseTimeM False defaultTimeLocale "%Y%m%d"

cleanTitle :: Text -> Text
cleanTitle = Text.strip . Text.takeWhile (/= '[')

red t = "\ESC[31;1;4m" <> t <> "\ESC[0m"

yellow t = "\ESC[33;1;4m" <> t <> "\ESC[0m"

green t = "\ESC[32;1;4m" <> t <> "\ESC[0m"

formatItem :: Day -> Int -> Item -> Text
formatItem today maxTitleLength item = [fmt| • {item.title: {maxTitleLength}} {bar} ({coloredDays} days)|]
  where
    daysLeft = diffDays item.dueDate today

    coloredDays
      | daysLeft < 0 = red (show daysLeft)
      | daysLeft < 8 = yellow (show daysLeft)
      | otherwise = show daysLeft

    bar = progressBar (daysLeft < 8) (28 - fromIntegral daysLeft) 28

main :: IO ()
main = do
  Just users <- decodeFileStrict @[User] "credentials.json"
  let names = fmap (.name) users

  peoples <- forM names $ \name -> do
    contentM <- decodeFileStrict @Payload ("result_" <> name <> ".json")

    case contentM of
      Nothing -> pure (name, [])
      Just content -> do
        let items = content.response.items
        pure (name, sortBy (comparing (.dueDate)) items)

  today <- (.utctDay) <$> getCurrentTime

  let maxTextLength = maximum $ do
        (_, items) <- peoples
        item <- items
        pure (Text.length item.title)

  forM_ peoples $ \(name, items) -> do
    putStrLn $ [fmt|{Text.toTitle $ Text.pack name} |] <> mkSummary (length items) 10
    forM_ items $ \item -> do
      Text.putStrLn $ formatItem today maxTextLength item

  putStrLn ""
  let total = sum $ map (\(_name, items) -> length items) peoples
      capacity = length peoples * 10

  putStrLn $ "Total: " <> mkSummary total capacity

mkSummary :: Int -> Int -> String
mkSummary total capacity = [fmt|{total}/{capacity}. Available: {capacity - total} ({pct:.1%}) {bar}|]
  where
    pct = fromIntegral @Int @Double total / fromIntegral capacity
    bar = progressBar True total capacity

fullBlock :: Char
fullBlock = '\x2588'

progressBar :: Bool -> Int -> Int -> [Char]
progressBar important a b = color (replicate a fullBlock) <> green (replicate (b - a) fullBlock)
  where
    color
      | important = red
      | otherwise = yellow
