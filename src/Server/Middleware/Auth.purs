module Server.Middleware.Auth
( AuthedRequest
, wrapTokenAuth
, underlyingRequest
, tokenPayload
)
where

import CustomPrelude

import Data.Map as M
import Data.Newtype (unwrap, wrap)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Server.Handler (Request, Response)

import Utils.ExceptT (ExceptT(..), runExceptT, liftEffectRight)
import Utils.JWT

data AuthedRequest b a = AuthedRequest b (Request a)

underlyingRequest :: forall a b. AuthedRequest b a -> Request a
underlyingRequest (AuthedRequest _ req) = req

tokenPayload :: forall a b. AuthedRequest b a -> b
tokenPayload (AuthedRequest payload _) = payload

retrieveToken :: forall a. Request a -> Either String JWT
retrieveToken req = case M.lookup (wrap "AuthToken") $ unwrap req.headers of
  (Just token) -> Right (wrap token)
  Nothing -> Left "No auth token in header"

orErrorResp :: forall res. (String -> Aff res) -> ExceptT String Aff res -> Aff res
orErrorResp res exceptT  = do
  e <- runExceptT exceptT
  case e of
    (Left err) -> res err
    (Right v) -> pure v

wrapTokenAuth :: forall res a b.
                 (JWT -> Effect (Either String a))
              -> (String -> Aff res)
              -> (AuthedRequest a b -> Aff res)
              -> Request b
              -> Aff res
wrapTokenAuth tokenVerifier authErrorResponse handler request =
  orErrorResp authErrorResponse do
    token <- ExceptT $ pure $ retrieveToken request
    tokenPayload <- ExceptT $ liftEffect $ tokenVerifier $ token
    ExceptT $ map Right $ handler (AuthedRequest tokenPayload request)
