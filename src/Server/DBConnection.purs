module Server.DBConnection (fromURI) where

import Prelude
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..))
import Data.Array as Arr
import Data.These (theseLeft, theseRight)
import Data.String.NonEmpty (toString)
import Database.PostgreSQL.PG as PG
import URI as U
import URI (AbsoluteURI)
import URI.Host as Host
import URI.Port as Port
import URI.HostPortPair as HostPortPair
import URI.HostPortPair (HostPortPair)
import URI.AbsoluteURI (AbsoluteURIOptions, parser, _hierPart, _userInfo, _authority, _path, _hosts)
import URI.Extra.UserPassInfo as UserPassInfo
import URI.Extra.UserPassInfo (UserPassInfo)
import URI.Path.Segment (segmentToString)
import Data.Lens as L
import Text.Parsing.Parser (runParser)

type PostgresURI
  = AbsoluteURI UserPassInfo (HostPortPair U.Host U.Port) U.Path U.HierPath U.Query

options ∷ Record (AbsoluteURIOptions UserPassInfo (HostPortPair U.Host U.Port) U.Path U.HierPath U.Query)
options =
  { parseUserInfo: UserPassInfo.parse
  , printUserInfo: UserPassInfo.print
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: pure
  , printQuery: identity
  }

showError :: forall err v. (Show err) => Either err v -> Either String v
showError (Left err) = Left $ show err

showError (Right v) = Right v

pathHead :: U.Path -> Maybe String
pathHead (U.Path segments) = segmentToString <$> (Arr.head segments)

host :: PostgresURI -> Maybe String
host uri = do
  hostPortPair <- join $ L.lastOf (_hierPart <<< _authority <<< _hosts) uri
  h <- theseLeft hostPortPair
  pure $ Host.print h

port :: PostgresURI -> Maybe Int
port uri = do
  hostPortPair <- join $ L.lastOf (_hierPart <<< _authority <<< _hosts) uri
  port' <- theseRight hostPortPair
  pure $ Port.toInt port'

database :: PostgresURI -> Either String String
database uri = case databaseM of
  (Just database') -> Right database'
  Nothing -> Left "URI doesn't have database specified"
  where
  databaseM = join $ pathHead <$> L.lastOf (_hierPart <<< _path) uri

user :: PostgresURI -> Maybe String
user uri = toString <$> _.user <$> unwrap <$> userInfo
  where
  userInfo = join $ L.lastOf (_hierPart <<< _authority <<< _userInfo) uri

password :: PostgresURI -> Maybe String
password uri = do
  userInfo <- userInfoM
  password' <- (unwrap userInfo).password
  pure $ toString password'
  where
  userInfoM = join $ L.lastOf (_hierPart <<< _authority <<< _userInfo) uri

-- postgres://username:password@host:port/path
uriToConfig :: PostgresURI -> Either String PG.PoolConfiguration
uriToConfig uri = do
  database' <- database uri
  pure
    $ { database: database'
      , host: host uri
      , idleTimeoutMillis: Nothing
      , max: Nothing
      , password: password uri
      , user: user uri
      , port: port uri
      }

fromURI :: String -> Either String PG.PoolConfiguration
fromURI s =
  showError do
    let
      p = parser options
    uri <- showError (runParser s p)
    uriToConfig uri
