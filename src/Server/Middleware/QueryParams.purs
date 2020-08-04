module Server.Middleware.QueryParams
( wrapParseQueryParams )
where

import Prelude

import Data.Either (Either(..))

import Prim.RowList as RL

import JohnCowie.HTTPure as Req
import JohnCowie.HTTPure.QueryParams (class GDecodeQueryParams, parseQueryParams)
import JohnCowie.Data.Lens as L

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))

wrapParseQueryParams :: forall a row list req res m.
                        GDecodeQueryParams row list
                     => RL.RowToList row list
                     => Bind m
                     => Applicative m
                     => Functor req
                     => Req.IsRequest req
                     => (Array String -> m res)
                     -> (req (Record row /\ a)
                     -> m res)
                     -> req a
                     -> m res
wrapParseQueryParams errorHandler handler req =
  case parseQueryParams (L.view Req._query req) of
    (Left errs) -> errorHandler errs
    (Right qp) -> handler $ map (Tuple qp) req
