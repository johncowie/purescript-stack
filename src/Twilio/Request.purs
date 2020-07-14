module Twilio.Request
( AuthToken
, Signature
, signature
, validateRequest )
where

import Prelude
import Data.Maybe (Maybe(..))
import Utils.Env (class ParseValue)

newtype AuthToken = AuthToken String

instance parseValueAuthToken :: ParseValue AuthToken where
  parseValue = AuthToken >>> Just

newtype Signature = Signature String
signature :: String -> Signature
signature = Signature

validateRequest :: AuthToken -> Signature -> String -> String -> Boolean
validateRequest (AuthToken authToken) (Signature sig) url body =
  _validateRequest authToken sig url body

foreign import _validateRequest :: String -> String -> String -> String -> Boolean
