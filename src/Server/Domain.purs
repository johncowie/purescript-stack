module Server.Domain where

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
