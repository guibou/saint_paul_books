{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Books where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import GHC.Generics

-- | A book loan
data BookStatus = Available | Waiting | DueAt Day
  deriving (Show, Generic, Eq, ToJSON, FromJSON, Ord)

data Book = Book
  { title :: Text,
    fullTitle :: Text,
    status :: BookStatus,
    author :: Text,
    cover :: Text,
    barcode :: Text,
    originalPayload :: Value
  }
  deriving (Show, Generic, Eq, ToJSON, FromJSON)

newtype JSONBook = JSONBook Book

instance FromJSON JSONBook where
  parseJSON value = JSONBook <$> bookFromJSON value

-- | What is actually stored in the book store
data BookStoreStatus = BookStoreStatus
  {
    -- | Number of rented books
    due :: Int,
    -- | Waiting for reservation
    waiting :: Int,
    -- | Reservation available
    available :: Int
  }
  deriving (Show)

foldStatus :: [Book] -> BookStoreStatus
foldStatus = flip foldl' (BookStoreStatus 0 0 0) $ \acc book ->
  case status book of
    DueAt {} -> acc {due = due acc + 1}
    Waiting -> acc {waiting = waiting acc + 1}
    Available -> acc {available = available acc + 1}

-- | This is used to parse a Book from the iguana json payload
-- The ToJSON / FromJSON instances for Book are used for internal API serialization
bookFromJSON :: Value -> Parser Book
bookFromJSON = withObject "Book" $ \o -> do
  title <- cleanTitle <$> o .: "title"
  dueDateM <- o .:? "dueDate"
  status <- case dueDateM of
    Just dueDate -> do
      due <- cleanDate dueDate
      pure (DueAt due)
    Nothing -> do
      availableSinceM <- o .:? "availableSince"
      case availableSinceM of
        Nothing -> pure Waiting
        Just (v :: Text) -> do
          pure Available

  author <- o .: "author"
  cover <- cleanImage <$> (o .: "image")
  fullTitle <- o .: "title"
  barcode <- o .: "barcode"
  let originalPayload = Object o
  pure $ Book {..}

cleanDate :: (MonadFail m) => String -> m Day
cleanDate = parseTimeM False defaultTimeLocale "%Y%m%d"

cleanTitle :: Text -> Text
cleanTitle = Text.strip . Text.takeWhile (/= '[')

cleanImage :: Text -> Text
cleanImage t = Text.takeWhile (/= '!') t
