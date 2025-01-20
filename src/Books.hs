{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Books where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, defaultTimeLocale)
import Data.Time.Format (formatTime, parseTimeM)
import GHC.Generics

-- | A book loan
data Book = Book
  { title :: Text,
    dueDate :: Day
  }
  deriving (Show, Generic, Eq)

instance FromJSON Book where
  parseJSON = withObject "Book" $ \o -> do
    Book <$> (cleanTitle <$> o .: "title") <*> (cleanDate =<< o .: "dueDate")

instance ToJSON Book where
  toJSON book =
    object
      [ "title" .= title book,
        "dueDate" .= formatTime defaultTimeLocale "%Y%m%d" (dueDate book)
      ]

cleanDate :: (MonadFail m) => String -> m Day
cleanDate = parseTimeM False defaultTimeLocale "%Y%m%d"

cleanTitle :: Text -> Text
cleanTitle = Text.strip . Text.takeWhile (/= '[')
