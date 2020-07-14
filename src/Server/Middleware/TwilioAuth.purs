module Server.Middleware.TwilioAuth
( TwilioRequest
, wrapTwilioAuth )
where

import Prelude

import Data.Either (Either(..), note)
import Data.Maybe (fromMaybe)
import Data.String as Str
import Server.Request (class Request)
import Server.Request as Req
import Twilio.Config (TwilioConfig)
import Twilio.Request (validateRequest, signature)
import Utils.ExceptT (booleanToError)
import Utils.Lens (type (:->))
import Utils.Lens as L

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

requestedUrl :: forall req a. (Request req) => req a -> String
requestedUrl req = "https://" <> host <> "/" <> path
  where host = fromMaybe "" $ Req.lookupHeader "Host" req
        path = Str.joinWith "/" $ L.view Req._path req

wrapTwilioAuth :: forall a res. TwilioConfig
               -> (String -> res)
               -> (TwilioRequest a -> res)
               -> (Req.BasicRequest a)
               -> res
wrapTwilioAuth config authErrorResponse router req = orError authErrorResponse do
  sig <- note "Missing twilio auth header" $ Req.lookupHeader "x-twilio-signature" req
  booleanToError "Unauthorised request" $ validateRequest authToken (signature sig) url body
  pure $ router (TwilioRequest req)
  where orError f (Left err) = f err
        orError f (Right v) = v
        authToken = config.authToken
        body = L.view Req._body req
        url = requestedUrl req
