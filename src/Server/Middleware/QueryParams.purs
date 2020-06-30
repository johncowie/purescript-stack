module Server.Middleware.QueryParams
( wrapParseQueryParams )
where

import Prelude

import Data.Either (Either(..))

import Prim.RowList as RL

import Server.Handler (Request, addToRequestVal)
import Server.QueryParams (class GDecodeQueryParams, parseQueryParams)

import Data.Tuple.Nested (type (/\))

wrapParseQueryParams :: forall a row list res m.
                        GDecodeQueryParams row list
                     => RL.RowToList row list
                     => Bind m
                     => Applicative m
                     => (String -> m res)
                     -> (Request (Record row /\ a)
                     -> m res)
                     -> Request a
                     -> m res
wrapParseQueryParams errorHandler handler req = case parseQueryParams req.query of
  (Left err) -> errorHandler err
  (Right qp) -> handler $ addToRequestVal qp req
