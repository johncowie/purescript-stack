module Utils.AppendStore
( AppendStore
, localStorageAppendStore
, httpAppendStore
, syncAppendStore)
where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (length, reverse)
import Data.HTTP.Method (Method(..))
import Effect.Exception (Error, error)
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

apiGet :: forall e. (JSON.DecodeJson e) => String -> Aff (Either Error e)
apiGet url = do
  result <- AX.request $ AX.defaultRequest { responseFormat = ResponseFormat.json
                                           , method = Left GET
                                           , url = url
                                           , headers = [RequestHeader "Authorization" "john:bobbydazzler"]}
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> case JSON.decodeJson response.body of
      Left jsonErr -> pure $ Left $ error jsonErr
      Right events -> pure $ Right $ events

apiPost :: forall e. (JSON.EncodeJson e) => String -> e -> Aff (Either Error Unit)
apiPost url e = do
  result <- AX.request $ AX.defaultRequest { responseFormat = ResponseFormat.json
                                           , method = Left POST
                                           , url = url
                                           , content = body
                                           , headers = [RequestHeader "Authorization" "john:bobbydazzler"]}
  case result of
    Left err -> pure $ Left $ error $ AX.printError err
    Right response -> pure $ Right unit
  where body = Just $ RequestBody.json $ JSON.encodeJson e

rootUrl :: String
rootUrl = "https://dumb-waiter.herokuapp.com"

appendHTTP :: forall e. (JSON.EncodeJson e) => String -> e -> Aff (Either Error Unit)
appendHTTP s event = apiPost (rootUrl <> "?app=" <> s) event

syncHTTP :: forall e. (JSON.EncodeJson e) => String -> Array e -> Aff (Either Error Unit)
syncHTTP s events = apiPost (rootUrl <> "/sync?app=" <> s) events

retrieveAllHTTP :: forall e. (JSON.DecodeJson e) => String -> Aff (Either Error (Array e))
retrieveAllHTTP s = apiGet (rootUrl <> "?app=" <> s)

httpAppendStore :: forall e. (JSON.DecodeJson e) => (JSON.EncodeJson e) => String -> AppendStore e
httpAppendStore k = {
  append: appendHTTP k
, retrieveAll: retrieveAllHTTP k
}

syncAppendStore :: forall e. (JSON.DecodeJson e) => (JSON.EncodeJson e) => String -> AppendStore e
syncAppendStore k = {append, retrieveAll}
  where append event = runExceptT do
          (eventsFromHTTP :: Array e) <- ExceptT $ retrieveAllHTTP k
          (eventsFromLocal :: Array e) <- ExceptT $ async $ retrieveFromLocalStorage k
          ExceptT $ Right <$> (async $ LS.store k ([event] <> eventsFromLocal))
          if length eventsFromHTTP == length eventsFromLocal
            then ExceptT $ appendHTTP k event
            else ExceptT $ syncHTTP k $ reverse ([event] <> eventsFromLocal)
        retrieveAll = async $ retrieveFromLocalStorage k
