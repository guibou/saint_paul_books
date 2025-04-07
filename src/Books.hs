{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Books where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import GHC.Generics

-- | A book loan
data Book = Book
  { title :: Text,
    fullTitle :: Text,
    dueDate :: Day,
    author :: Text,
    cover :: Text,
    barcode :: Text
  }
  deriving (Show, Generic, Eq, ToJSON, FromJSON)

newtype JSONBook = JSONBook Book

instance FromJSON JSONBook where
  parseJSON value = JSONBook <$> bookFromJSON value

-- | This is used to parse a Book from the iguana json payload
-- The ToJSON / FromJSON instances for Book are used for internal API serialization
bookFromJSON :: Value -> Parser Book
bookFromJSON = withObject "Book" $ \o -> do
  title <- cleanTitle <$> o .: "title"
  dueDate <- cleanDate =<< o .: "dueDate"
  author <- o .: "author"
  cover <- cleanImage <$> (o .: "image")
  fullTitle <- o .: "title"
  barcode <- o .: "barcode"
  pure $ Book {..}

cleanDate :: (MonadFail m) => String -> m Day
cleanDate = parseTimeM False defaultTimeLocale "%Y%m%d"

cleanTitle :: Text -> Text
cleanTitle = Text.strip . Text.takeWhile (/= '[')

cleanImage :: Text -> Text
cleanImage t = Text.takeWhile (/= '!') t
