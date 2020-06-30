module Server.Handler
( Request
, Response
, updateRequestVal
, updateRequestValM
, addToRequestVal
, response
, redirect
, emptyResponse
, wrapCustom
, addResponseHeader
)
where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Data.Symbol (SProxy(..))
import Data.Map as M
import Data.Newtype (wrap)

import Effect.Aff (Aff)

import HTTPure as HP
import HTTPure.Headers as Headers
import HTTPure.Version (Version)

import Utils.Lens as L

type Request a = { headers :: HP.Headers
                 , httpVersion :: Version
                 , method :: HP.Method
                 , path :: HP.Path
                 , query :: HP.Query
                 , body :: String
                 , val :: a }

type Response a = { headers :: HP.Headers
                  , status :: HP.Status
                  , body :: a }

updateRequestVal :: forall a b. (a -> b) -> Request a -> Request b
updateRequestVal f {headers, httpVersion, method, path, query, body, val}
  = {headers, httpVersion, method, path, query, body, val: (f val)}

addToRequestVal :: forall a b. b -> Request a -> Request (b /\ a)
addToRequestVal val = updateRequestVal (Tuple val)

updateRequestValM :: forall a m b. (Bind m) => (Applicative m) => (a -> m b) -> Request a -> m (Request b)
updateRequestValM f {headers, httpVersion, method, path, query, body, val} = do
  updated <- f val
  pure {headers, httpVersion, method, path, query, body, val: updated}

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

toCustomRequest :: HP.Request -> Request Unit
toCustomRequest {headers, httpVersion, method, path, query, body}
  = {headers, httpVersion, method, path, query, body, val: unit}

fromCustomResponse :: Response String -> Aff HP.Response
fromCustomResponse r = do
  res <- HP.response r.status r.body
  pure $ res {headers = r.headers}

wrapCustom :: (Request Unit -> Aff (Response String)) -> HP.Request -> Aff HP.Response
wrapCustom router request = do
  res <- router (toCustomRequest request)
  fromCustomResponse res
