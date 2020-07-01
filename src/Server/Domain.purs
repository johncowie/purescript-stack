module Server.Domain where

import Prelude

import Data.Newtype (class Newtype)

import Server.QueryParams (class ParseQueryParam, parseNewtype)

newtype AppName = AppName String
derive instance newtypeAppName :: Newtype AppName _

instance parseQueryParamAppName :: ParseQueryParam AppName where
  parseQueryParam = parseNewtype

newtype EventId = EventId Int
derive instance newtypeEventId :: Newtype EventId _

instance parseQueryParamEventId :: ParseQueryParam EventId where
  parseQueryParam = parseNewtype

newtype UserId = UserId Int
derive instance newtypeUserId :: Newtype UserId _
derive instance eqUserId :: Eq UserId
instance showUserId :: Show UserId where
  show (UserId i) = show i

data OAuthProvider = Google | Stub
instance showOAuthProvider :: Show OAuthProvider where
  show Google = "Google"
  show Stub = "Stub"

type NewUser = {
  thirdParty :: OAuthProvider
, thirdPartyId :: String
, name :: String
}

type User = {
  id :: UserId
, name :: String
}
