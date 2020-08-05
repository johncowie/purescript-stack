module Utils.HttpClient where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.HTTP.Method (Method(GET, POST))

import Effect.Aff (Aff)
import Effect.Exception (Error, error)

toError :: AX.Error -> Error
toError = AX.printError >>> error

decodeError :: Json -> String -> Error
decodeError body errMsg = error $ errMsg <> "\n" <> "Body was: " <> stringify body

jsonRequest_ :: forall a. AX.Request Json -> (Json -> Aff (Either String a)) -> Aff (Either Error a)
jsonRequest_ req decoder = runExceptT do
  response <- ExceptT $ map (lmap toError) $ AX.request req
  ExceptT $ map (lmap (decodeError response.body)) $ decoder (response.body)

getJson_ :: forall a. (DecodeJson a) => (AX.Request Json -> AX.Request Json) -> String -> Aff (Either Error a)
getJson_ requestUpdate url = jsonRequest_ request decoder
  where request = requestUpdate $ AX.defaultRequest { responseFormat = ResponseFormat.json
                                                    , method = Left GET
                                                    , url = url }
        decoder = pure <<< decodeJson

getJson :: forall a. (DecodeJson a) => String -> Aff (Either Error a)
getJson = getJson_ identity

postReturnJson :: forall a. (DecodeJson a) => String -> RequestBody.RequestBody -> Aff (Either Error a)
postReturnJson url body = jsonRequest_ request (pure <<< decodeJson)
  where request = AX.defaultRequest { responseFormat = ResponseFormat.json
                                    , method = Left POST
                                    , url = url
                                    , content = Just body}
