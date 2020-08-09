module Twilio.Request
  ( AuthToken
  , Signature
  , signature
  , validateRequest
  ) where

import Prelude
import Envisage.Var (class ParseValue)
import Foreign.Object (Object)

newtype AuthToken
  = AuthToken String

instance parseValueAuthToken :: ParseValue AuthToken where
  parseValue s = pure (AuthToken s)

newtype Signature
  = Signature String

signature :: String -> Signature
signature = Signature

validateRequest :: AuthToken -> Signature -> String -> (Object String) -> Boolean
validateRequest (AuthToken authToken) (Signature sig) url body = _validateRequest authToken sig url body

foreign import _validateRequest :: String -> String -> String -> (Object String) -> Boolean
