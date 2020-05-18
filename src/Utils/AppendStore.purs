module Utils.AppendStore
( AppendStore
, localStorageAppendStore )
where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Data.Argonaut.Decode (class DecodeJson) as JSON
import Data.Argonaut.Encode (class EncodeJson) as JSON
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Effect.Exception (Error)
import Utils.LocalJsonStorage as LS
import Utils.Async (async)

type AppendStore e = {
  append :: e -> Aff (Either Error Unit)
, retrieveAll :: Aff (Either Error (Array e))
}

appendToLocalStorage :: forall e. (JSON.DecodeJson e) => (JSON.EncodeJson e) => String -> e -> Effect (Either Error Unit)
appendToLocalStorage s e = do
  eventsE <- retrieveFromLocalStorage s
  case eventsE of
    (Left err) -> pure (Left err)
    (Right events) -> Right <$> LS.store s ([e] <> events)

retrieveFromLocalStorage :: forall e. (JSON.DecodeJson e) => String -> Effect (Either Error (Array e))
retrieveFromLocalStorage s = do
  eventsEM <- LS.load s
  pure $ fromMaybe [] <$> eventsEM

localStorageAppendStore :: forall e. (JSON.DecodeJson e) => (JSON.EncodeJson e) => String -> AppendStore e
localStorageAppendStore k = {
  append: \e -> async $ appendToLocalStorage k e
, retrieveAll: async $ retrieveFromLocalStorage k
}
