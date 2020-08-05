module Server.Domain where

import Prelude
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import JohnCowie.HTTPure.QueryParams (class ParseQueryParam, parseNewtype)

newtype AppName
  = AppName String

derive instance newtypeAppName :: Newtype AppName _

instance parseQueryParamAppName :: ParseQueryParam AppName where
  parseQueryParam = parseNewtype

newtype EventId
  = EventId Int

derive instance newtypeEventId :: Newtype EventId _

instance encodeJsonEventId :: EncodeJson EventId where
  encodeJson = unwrap >>> encodeJson

instance decodeJsonEventId :: DecodeJson EventId where
  decodeJson = decodeJson >>> map wrap

instance parseQueryParamEventId :: ParseQueryParam EventId where
  parseQueryParam = parseNewtype

newtype UserId
  = UserId Int

derive instance newtypeUserId :: Newtype UserId _

derive instance eqUserId :: Eq UserId

instance showUserId :: Show UserId where
  show (UserId i) = show i

instance encodeJsonUserId :: EncodeJson UserId where
  encodeJson = unwrap >>> encodeJson

instance decodeJsonUserId :: DecodeJson UserId where
  decodeJson = decodeJson >>> map wrap

data OAuthProvider
  = Google
  | Stub
  | WhatsApp

instance showOAuthProvider :: Show OAuthProvider where
  show Google = "Google"
  show Stub = "Stub"
  show WhatsApp = "WhatsApp"

type NewUser
  = { thirdParty :: OAuthProvider
    , thirdPartyId :: String
    , name :: String
    }

type User
  = { id :: UserId
    , name :: String
    }

newtype Token
  = Token String

derive instance newtypeToken :: Newtype Token _

derive instance eqToken :: Eq Token

instance showToken :: Show Token where
  show (Token token) = token
