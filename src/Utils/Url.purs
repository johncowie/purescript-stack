module Utils.Url
( getWindowUrl
, getQueryParams
)
where

import Prelude
import Effect (Effect)
-- import Effect.Exception.Unsafe (unsafeThrow)
import Web.HTML (window) as DOM
import Web.HTML.Window (location) as DOM
import Web.HTML.Location (href) as DOM
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Array as Array

getWindowUrl :: Effect String
getWindowUrl = do
  window <- DOM.window
  location <- DOM.location window
  DOM.href location

queryTuple :: String -> Maybe (Tuple String String)
queryTuple s = case String.split (Pattern "=") s of
  [k, v] -> Just (Tuple k v)
  otherwise -> Nothing

getQueryParams :: String -> Map String String
getQueryParams s = Maybe.fromMaybe Map.empty $ do
  queryStr <- Array.last $ String.split (Pattern "?") s
  let queries = String.split (Pattern "&") queryStr
      queryTuples = Array.catMaybes $ map queryTuple queries
  pure $ Map.fromFoldable queryTuples
