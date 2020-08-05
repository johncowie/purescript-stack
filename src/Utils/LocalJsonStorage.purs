module Utils.LocalJsonStorage
  ( load
  , store
  ) where

import Prelude
import Effect (Effect)
import Effect.Exception (Error, error)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Web.Storage.Storage (getItem, setItem) as Storage
import Data.Argonaut.Parser (jsonParser) as JSON
import Data.Argonaut.Core (stringify) as JSON
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Web.HTML (window) as DOM
import Web.HTML.Window (localStorage) as DOM

wrapError :: forall a. Either String a -> Either Error a
wrapError (Left s) = Left (error s)

wrapError (Right s) = (Right s)

load :: forall a. (JSON.DecodeJson a) => String -> Effect (Either Error (Maybe a))
load storageKey =
  wrapError
    <$> do
        window <- DOM.window
        localStorage <- DOM.localStorage window
        jsonM <- Storage.getItem storageKey localStorage
        pure
          $ case jsonM of
              (Just jsonStr) -> do
                json <- JSON.jsonParser jsonStr
                JSON.decodeJson json
              (Nothing) -> Right Nothing

store :: forall a. (JSON.EncodeJson a) => String -> a -> Effect Unit
store storageKey val = do
  window <- DOM.window
  localStorage <- DOM.localStorage window
  let
    json = JSON.stringify $ JSON.encodeJson val
  Storage.setItem storageKey json localStorage
