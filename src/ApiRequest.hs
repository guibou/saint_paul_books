{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module ApiRequest where

import Books
import Data.Aeson
import Data.Text
import GHC.Generics
import Data.Time (Day)

data RenewResponse = RenewResponse
  { -- book barcode, useful as an entrypoint
    barcode :: Text,
    -- The new due date. The book could be updated
    dueDate :: Day,
    refusalReason :: Text,
    success :: Bool
  }
  deriving (Show, Generic)

instance FromJSON RenewResponse where
  parseJSON = withObject "RenewResponse" $ \o -> do
    refusalReason <- o .: "refusalReason"
    dueDate <- cleanDate =<< o .: "dueDate"
    barcode <- o .: "barcode"
    successInt <- o .: "success"
    let success = case successInt of
           (1 :: Int) -> True
           _ -> False
    pure $ RenewResponse {..}

data Results x = Results {results :: x}
  deriving (Show, Generic, FromJSON)
