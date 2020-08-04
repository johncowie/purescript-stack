module JohnCowie.HTTPure
( class IsRequest
, _headers
, _httpVersion
, _method
, _path
, _query
, _body
, _val
, BasicRequest
, Response
, response
, addResponseHeader
, setContentType
, serve'
, lookupHeader
)
where

import Prelude

import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Symbol (SProxy(..))
import Data.Map as M
import Data.Maybe (Maybe)

import Effect (Effect)
import Effect.Aff (Aff)

import HTTPure as HP
import HTTPure.Headers as Headers
import HTTPure.Version (Version)

import Node.HTTP as HTTP

import JohnCowie.Data.Lens as L
import JohnCowie.Data.Lens (type (:->))

class IsRequest req where
  _headers :: forall a. req a :-> HP.Headers
  _httpVersion :: forall a. req a :-> Version
  _method :: forall a. req a :-> HP.Method
  _path :: forall a. req a :-> Array String
  _query :: forall a. req a :-> HP.Query
  _body :: forall a. req a :-> String
  _val :: forall a. req a :-> a

newtype BasicRequest a = BasicRequest {
  headers :: HP.Headers
, httpVersion :: Version
, method :: HP.Method
, path :: Array String
, query :: HP.Query
, body :: String
, val :: a
}

derive instance functorBasicRequest :: Functor BasicRequest
derive instance newtypeBasicRequest :: Newtype (BasicRequest a) _

instance requestBasicRequest :: IsRequest BasicRequest where
  _headers = L._newtype >>> L.prop (SProxy :: SProxy "headers")
  _httpVersion = L._newtype >>> L.prop (SProxy :: SProxy "httpVersion")
  _method = L._newtype >>> L.prop (SProxy :: SProxy "method")
  _path = L._newtype >>> L.prop (SProxy :: SProxy "path")
  _query = L._newtype >>> L.prop (SProxy :: SProxy "query")
  _body = L._newtype >>> L.prop (SProxy :: SProxy "body")
  _val = L._newtype >>> L.prop (SProxy :: SProxy "val")

toCustomRequest :: HP.Request -> BasicRequest Unit
toCustomRequest {headers, httpVersion, method, path, query, body}
  = BasicRequest {headers, httpVersion, method, path, query, body, val: unit}

lookupHeader :: forall req a. (IsRequest req) => String -> req a -> Maybe String
lookupHeader k = L.view _headers >>> unwrap >>> M.lookup (wrap k)

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

_responseHeaders :: forall r. L.Lens' {headers :: HP.Headers | r} HP.Headers
_responseHeaders = L.prop (SProxy :: SProxy "headers")

addResponseHeader :: forall r. String -> String -> {headers :: HP.Headers | r} -> {headers :: HP.Headers | r}
addResponseHeader k v = L.over (_responseHeaders >>> L._newtype) (M.insert (wrap k) v)

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

serve' :: HTTP.ListenOptions
       -> (BasicRequest Unit -> Aff (Response String))
       -> Effect Unit
       -> HP.ServerM
serve' options handler onStarted = HP.serve' options (wrapCustom handler) onStarted
