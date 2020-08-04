module Server.Middleware.Auth
( AuthedRequest
, wrapTokenAuth
, tokenPayload
)
where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Map as M
import Data.Newtype (class Newtype, unwrap, wrap)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Server.Request (class Request)
import Server.Request as Req

import Utils.ExceptT (ExceptT(..), runExceptT)
import Utils.Lens as L
import Utils.Lens (type (:->))

data AuthedRequest tokenPayload a = AuthedRequest tokenPayload (Req.BasicRequest a)

instance requestAuthedRequest :: Request (AuthedRequest tp) where
  _headers = _underlyingRequest >>> Req._headers
  _httpVersion = _underlyingRequest >>> Req._httpVersion
  _method = _underlyingRequest >>> Req._method
  _path = _underlyingRequest >>> Req._path
  _query = _underlyingRequest >>> Req._query
  _body = _underlyingRequest >>> Req._body
  _val = _underlyingRequest >>> Req._val

derive instance functorAuthedRequest :: Functor (AuthedRequest tp)

_underlyingRequest :: forall tp a. AuthedRequest tp a :-> Req.BasicRequest a
_underlyingRequest = L.lens getter setter
  where getter (AuthedRequest tp req) = req
        setter (AuthedRequest tp _) req = AuthedRequest tp req

tokenPayload :: forall tp a. AuthedRequest tp a -> tp
tokenPayload (AuthedRequest payload _) = payload

retrieveToken :: forall req a token. (Request req) => (Newtype token String) => req a -> Either String token
retrieveToken req = case M.lookup (wrap "AuthToken") headers of
  (Just token) -> Right (wrap token)
  Nothing -> Left "No auth token in header"
  where headers = unwrap (L.view Req._headers req)

orErrorResp :: forall res. (String -> Aff res) -> ExceptT String Aff res -> Aff res
orErrorResp res exceptT  = do
  e <- runExceptT exceptT
  case e of
    (Left err) -> res err
    (Right v) -> pure v

wrapTokenAuth :: forall res a b token.
                 (Newtype token String)
              => (token -> Effect (Either String a))
              -> (String -> Aff res)
              -> (AuthedRequest a b -> Aff res)
              -> Req.BasicRequest b
              -> Aff res
wrapTokenAuth tokenVerifier authErrorResponse handler request =
  orErrorResp authErrorResponse do
    token <- ExceptT $ pure $ retrieveToken request
    tp <- ExceptT $ liftEffect $ tokenVerifier $ token
    ExceptT $ map Right $ handler (AuthedRequest tp request)
