module Utils.Url
( getWindowUrl
, getQueryParams
, getQueryParam
, getPath
, redirect
)
where

import Prelude
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Web.HTML (window) as DOM
import Web.HTML.Window (location) as DOM
import Web.HTML.Location (href, replace) as DOM
import Data.Either (either)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Data.String as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty

type URL = String

redirect :: String -> Effect Unit
redirect url = do
  window <- DOM.window
  location <- DOM.location window
  DOM.replace url location

getWindowUrl :: Effect URL
getWindowUrl = do
  window <- DOM.window
  location <- DOM.location window
  DOM.href location

queryTuple :: String -> Maybe (Tuple String String)
queryTuple s = case String.split (Pattern "=") s of
  [k, v] -> Just (Tuple k v)
  otherwise -> Nothing

getQueryParams :: URL -> Map String String
getQueryParams s = Maybe.fromMaybe Map.empty $ do
  queryStr <- Array.last $ String.split (Pattern "?") s
  let queries = String.split (Pattern "&") queryStr
      queryTuples = Array.catMaybes $ map queryTuple queries
  pure $ Map.fromFoldable queryTuples

getQueryParam :: String -> URL -> Maybe String
getQueryParam k url = Map.lookup k $ getQueryParams url

remove :: String -> String -> String
remove pattern = String.replaceAll (Pattern pattern) (Replacement "")

stripScheme :: String -> String
stripScheme = remove "https://" >>> remove "http://"

regexUnsafe :: String -> Flags.RegexFlags -> Regex.Regex
regexUnsafe s flags = either unsafeThrow identity $ Regex.regex s flags

getPath :: String -> Array String
getPath s = map (remove "/") $ Maybe.maybe [] NonEmpty.catMaybes $ Regex.match r $ stripScheme s
  where r = regexUnsafe "\\/([^\\/\\?]+)" Flags.global
