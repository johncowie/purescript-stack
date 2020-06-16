module Server.DB where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array (head)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import Data.Argonaut.Core (Json)

import Database.PostgreSQL.PG as PG
import Database.PostgreSQL.Row (Row1(Row1))

import Server.DBConnection (fromURI)
import Server.Domain (AppName, EventId)

type PG a = ExceptT PG.PGError Aff a
type Pool = PG.Pool

withConnection :: forall a. PG.Pool -> (PG.Connection -> PG a) -> PG a
withConnection = PG.withConnection runExceptT

withTransaction :: forall a. PG.Connection -> PG a -> PG a
withTransaction = PG.withTransaction runExceptT

createConnectionPool :: PG.PoolConfiguration -> Effect PG.Pool
createConnectionPool poolConfig = PG.newPool
  (poolConfig { idleTimeoutMillis = Just 1000 })

runQuery :: forall a. PG.Pool -> (PG.Connection -> PG a) -> Aff (Either PG.PGError a)
runQuery pool query = runExceptT do
  withConnection pool $ \conn -> do
    withTransaction conn $ query conn

addEvent :: String -> Json -> PG.Pool -> Aff (Either PG.PGError Int)
addEvent app event pool = runQuery pool \conn -> do
  rows <- PG.query conn (PG.Query """
    INSERT INTO EVENTS (id, app, event)
    VALUES (
	      (SELECT COALESCE(MAX(id), 0) + 1 FROM events)
      , $1
      , $2
    )
    RETURNING ID;
  """) (app /\ event)
  pure $ fromMaybe 0 $ head $ map (\(Row1 id) -> id) rows

retrieveEvents :: AppName -> Maybe EventId -> PG.Pool -> Aff (Either PG.PGError (Array (Tuple Int Json)))
retrieveEvents app eventIdM pool = runQuery pool \conn -> do
  PG.query conn (PG.Query """
    SELECT id, event FROM events
    WHERE app = $1
    AND id > $2
    ORDER BY id desc;
    """) (unwrap app /\ maybe 0 unwrap eventIdM)

retrieveLatestSnapshot :: String -> PG.Pool -> Aff (Either PG.PGError (Maybe (Tuple Json Int)))
retrieveLatestSnapshot app pool = runQuery pool \conn -> do
  rows <- PG.query conn (PG.Query """
    SELECT snapshot, up_to_event
    FROM snapshots
    WHERE app = $1
    ORDER BY up_to_event DESC
    LIMIT 1;
  """) (Row1 app)
  pure $ head rows

insertSnapshot :: String -> Json -> Int -> PG.Pool -> Aff (Either PG.PGError Unit)
insertSnapshot app snapshot upToEvent pool = runQuery pool \conn -> do
  PG.execute conn (PG.Query """
    INSERT INTO snapshots (id, app, snapshot, upToEvent)
    VALUES (
      (select coalesce(max(id), 0) + 1 from shapshots)
    , $1
    , $2
    , $3
    );
  """) (app /\ snapshot /\ upToEvent)

connectionMsg :: PG.PoolConfiguration -> String
connectionMsg poolConfig = "Connected to database " <> db <> " at " <> hostAndPort
  where db = poolConfig.database
        host = fromMaybe "" poolConfig.host
        port = fromMaybe "" $ show <$> poolConfig.port
        hostAndPort = host <> ":" <> port

showDBError :: forall a. Either PG.PGError a -> Either String a
showDBError (Left err) = Left (show err)
showDBError (Right a) = Right a

getDB :: String -> Effect (Either String PG.Pool)
getDB dbUri = case fromURI dbUri of
  (Left err) -> pure (Left err)
  (Right poolConfig) -> do
    pool <- createConnectionPool poolConfig
    Console.log $ connectionMsg poolConfig
    pure $ Right pool
