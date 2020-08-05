module Dunbar.ChatBot.Parsers
  ( parseDuration
  , showDuration
  , parseDate
  ) where

import Prelude
import Data.DateTime.Instant (Instant, fromDateTime)
import Data.Either (hush)
import Data.Formatter.DateTime as F
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as Str
import Utils.String (stripPunctuation, words)

tokens :: String -> Array String
tokens = words >>> map (stripPunctuation >>> Str.toLower)

parseAmountWord :: String -> Maybe Int
parseAmountWord "one" = Just 1

parseAmountWord "two" = Just 2

parseAmountWord "three" = Just 3

parseAmountWord "four" = Just 4

parseAmountWord "five" = Just 5

parseAmountWord "six" = Just 6

parseAmountWord "seven" = Just 7

parseAmountWord "eight" = Just 8

parseAmountWord "nine" = Just 9

parseAmountWord "ten" = Just 10

parseAmountWord _ = Nothing

parseAmount :: String -> Maybe Int
parseAmount s = do
  i <- Int.fromString s
  if (i > 0) then Just i else parseAmountWord s

parseTimeUnit :: String -> Maybe Int
parseTimeUnit "day" = Just 1

parseTimeUnit "days" = Just 1

parseTimeUnit "week" = Just 7

parseTimeUnit "weeks" = Just 7

parseTimeUnit "month" = Just 30

parseTimeUnit "months" = Just 30

parseTimeUnit "year" = Just 365

parseTimeUnit "years" = Just 365

parseTimeUnit _ = Nothing

parseDuration :: Array String -> Maybe Int
parseDuration [ "every", unit ] = parseTimeUnit unit

parseDuration [ amount, unit ] = do
  a <- parseAmount amount
  u <- parseTimeUnit unit
  pure $ a * u

parseDuration [ "every", amount, unit ] = parseDuration [ amount, unit ]

parseDuration _ = Nothing

showDuration :: Int -> String
showDuration i
  | i > 90 = show (i `div` 30) <> " months"
  | i > 30 = show (i `div` 7) <> " weeks"
  | otherwise = show i <> " days"

parseDate :: String -> Maybe Instant
parseDate s = case words s of
  [ w ] -> (F.unformatDateTime "DD/MM/YYYY" >>> hush >>> map fromDateTime) w
  _ -> Nothing
