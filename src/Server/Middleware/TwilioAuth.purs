module Server.Middleware.TwilioAuth
( TwilioRequest
, wrapTwilioAuth )
where

import Prelude

import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.FormURLEncoded (FormURLEncoded, toArray)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Foreign.Object (Object)
import Foreign.Object as Object

import Server.Request (class Request)
import Server.Request as Req
import Twilio.Config (TwilioConfig)
import Twilio.Request (validateRequest, signature)
import Utils.ExceptT (booleanToError, ExceptT(..), runExceptT, liftEffectRight)
import Utils.Lens (type (:->))
import Utils.Lens as L

import Undefined (undefined)

data TwilioRequest a = TwilioRequest (Req.BasicRequest a)

instance requestAuthedRequest :: Request TwilioRequest where
  _headers = _underlyingRequest >>> Req._headers
  _httpVersion = _underlyingRequest >>> Req._httpVersion
  _method = _underlyingRequest >>> Req._method
  _path = _underlyingRequest >>> Req._path
  _query = _underlyingRequest >>> Req._query
  _body = _underlyingRequest >>> Req._body
  _val = _underlyingRequest >>> Req._val

derive instance functorAuthedRequest :: Functor TwilioRequest

_underlyingRequest :: forall a. TwilioRequest a :-> Req.BasicRequest a
_underlyingRequest = L.lens getter setter
  where getter (TwilioRequest req) = req
        setter (TwilioRequest _) req = TwilioRequest req

-- middleware for adding url to request
requestedUrl :: forall req a. (Request req) => req a -> String
requestedUrl req = "https://" <> host <> "/" <> path
  where host = fromMaybe "" $ Req.lookupHeader "Host" req
        path = Str.joinWith "/" $ L.view Req._path req

orErrorResp :: forall res. (String -> Aff res) -> ExceptT String Aff res -> Aff res
orErrorResp res exceptT  = do
  e <- runExceptT exceptT
  case e of
    (Left err) -> res err
    (Right v) -> pure v

formToObject :: FormURLEncoded -> Object String
formToObject formData = foldr insertInObject Object.empty (toArray formData)
  where insertInObject (Tuple k (Just v)) o = Object.insert k v o
        insertInObject (Tuple k Nothing) o = o

wrapTwilioAuth :: forall a res. TwilioConfig
               -> (String -> Aff res)
               -> (TwilioRequest (FormURLEncoded /\ a) -> Aff res)
               -> (Req.BasicRequest (FormURLEncoded /\ a))
               -> Aff res
wrapTwilioAuth config authErrorResponse router req = orErrorResp authErrorResponse do
  sig <- ExceptT $ pure $ note "Missing twilio auth header" $ Req.lookupHeader "x-twilio-signature" req
  ExceptT $ pure $ booleanToError "Unauthorised request" $ validateRequest authToken (signature sig) url body
  ExceptT $ map Right $ router (TwilioRequest req)
  where authToken = config.authToken
        body = formToObject $ fst $ (L.view Req._val req)
        url = requestedUrl req
