module Server.Handler
( Response
, response
, redirect
, emptyResponse
, wrapCustom
, addResponseHeader
, setContentType
)
where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Data.Symbol (SProxy(..))
import Data.Map as M
import Data.Newtype (class Newtype, wrap)

import Effect.Aff (Aff)

import HTTPure as HP
import HTTPure.Headers as Headers

import HTTPure.Version (Version)

import Utils.Lens as L

import Server.Request (class Request, BasicRequest, toCustomRequest)

type Response a = { headers :: HP.Headers
                  , status :: HP.Status
                  , body :: a }

response :: forall a. HP.Status -> a -> Response a
response status body = {headers: Headers.empty, status, body}

-- TODO return headers so that redirect is not cached
redirect :: forall a. (Monoid a) => String -> Response a
redirect url = addResponseHeader "Location" url $ response 301 mempty

emptyResponse :: HP.Status -> Response Unit
emptyResponse status = response status unit

_headers :: forall r. L.Lens' {headers :: HP.Headers | r} HP.Headers
_headers = L.prop (SProxy :: SProxy "headers")

addResponseHeader :: forall r. String -> String -> {headers :: HP.Headers | r} -> {headers :: HP.Headers | r}
addResponseHeader k v = L.over (_headers >>> L._newtype) (M.insert (wrap k) v)

setContentType :: forall r. String -> {headers :: HP.Headers | r} -> {headers :: HP.Headers | r}
setContentType = addResponseHeader "Content-Type"

fromCustomResponse :: Response String -> Aff HP.Response
fromCustomResponse r = do
  res <- HP.response r.status r.body
  pure $ res {headers = r.headers}

wrapCustom :: (BasicRequest Unit -> Aff (Response String)) -> HP.Request -> Aff HP.Response
wrapCustom router request = do
  res <- router (toCustomRequest request)
  fromCustomResponse res
