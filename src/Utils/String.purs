module Utils.String where

import Data.String as Str
import Data.String.Regex (replace, split) as Re
import Data.String.Regex.Flags (global) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re

words :: String -> Array String
words = Re.split regex
  where
  regex = Re.unsafeRegex "\\s+" Re.global

stripPunctuation :: String -> String
stripPunctuation = Re.replace regex ""
  where
  regex = Re.unsafeRegex "[^a-zA-Z0-9\\s]" Re.global

unwords :: Array String -> String
unwords = Str.joinWith " "

unlines :: Array String -> String
unlines = Str.joinWith "\n"
