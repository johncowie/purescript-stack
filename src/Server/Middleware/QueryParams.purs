module Server.Middleware.QueryParams
( wrapParseQueryParams )
where

import Prelude

import Data.Either (Either(..))

import Prim.RowList as RL

import Server.Request as Req
import Server.QueryParams (class GDecodeQueryParams, parseQueryParams)

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Utils.Lens as L

wrapParseQueryParams :: forall a row list req res m.
                        GDecodeQueryParams row list
                     => RL.RowToList row list
                     => Bind m
                     => Applicative m
                     => Functor req
                     => Req.Request req
                     => (String -> m res)
                     -> (req (Record row /\ a)
                     -> m res)
                     -> req a
                     -> m res
wrapParseQueryParams errorHandler handler req =
  case parseQueryParams (L.view Req._query req) of
    (Left err) -> errorHandler err
    (Right qp) -> handler $ map (Tuple qp) req
