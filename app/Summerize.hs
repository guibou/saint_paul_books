{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

import Api
import ApiRequest
import Books
import Control.Monad (forM_)
import Data.Aeson
import Data.Coerce
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
import PyF
import System.Environment (getArgs)

red, yellow, green :: String -> String
red t = "\ESC[31;1;4m" <> t <> "\ESC[0m"
yellow t = "\ESC[33;1;4m" <> t <> "\ESC[0m"
green t = "\ESC[32;1;4m" <> t <> "\ESC[0m"

formatItem :: Day -> Int -> Book -> Text
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
  -- Check if we need to refresh
  args <- getArgs
  case args of
    ["--refresh"] -> do
      refreshIguanaFile
    _ -> pure ()

  Just (coerce -> peoples) <- decodeFileStrict @[(Text, [JSONBook])] ".iguana.json"

  today <- (.utctDay) <$> getCurrentTime

  let maxTextLength = maximum $ do
        (_, items) <- peoples
        item <- items
        pure (Text.length item.title)

  forM_ peoples $ \(name, items) -> do
    putStrLn $ [fmt|{Text.toTitle name} |] <> mkSummary (length items) 10
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

refreshIguanaFile :: IO ()
refreshIguanaFile = do
  Just users <- decodeFileStrict @[User] "credentials.json"
  items <- refresh users
  encodeFile ".iguana.json" items
