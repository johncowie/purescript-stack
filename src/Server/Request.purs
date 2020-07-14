module Server.Request
( class Request
, _headers
, _httpVersion
, _method
, _path
, _query
, _body
, _val
, BasicRequest
, toCustomRequest
, lookupHeader
)
where

import Prelude
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Symbol (SProxy(..))
import Data.Map as M
import Data.Maybe (Maybe)
import HTTPure as HP
import HTTPure.Headers as Headers
import HTTPure.Version (Version)
import Utils.Lens as L
import Utils.Lens (type (:->))

class Request req where
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

instance requestBasicRequest :: Request BasicRequest where
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

lookupHeader :: forall req a. (Request req) => String -> req a -> Maybe String
lookupHeader k = L.view _headers >>> unwrap >>> M.lookup (wrap k)
