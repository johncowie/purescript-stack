module Server.DB where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (errorShow)
import Effect.Aff (Aff, runAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson) as JSON

import Database.PostgreSQL.PG as PG
import Database.PostgreSQL.Row (Row1(Row1))

import Server.DBConnection (fromURI)

type PG a = ExceptT PG.PGError Aff a
type Pool = PG.Pool

withConnection :: forall a. PG.Pool -> (PG.Connection -> PG a) -> PG a
withConnection = PG.withConnection runExceptT

withTransaction :: forall a. PG.Connection -> PG a -> PG a
withTransaction = PG.withTransaction runExceptT

createConnectionPool :: PG.PoolConfiguration -> Effect PG.Pool
createConnectionPool poolConfig = PG.newPool
  (poolConfig { idleTimeoutMillis = Just 1000 })

-- createEventsTable :: PG.Connection -> PG Unit
-- createEventsTable conn = PG.execute conn (PG.Query """
--   CREATE TABLE events (
--     id SERIAL PRIMARY KEY,
--     app TEXT NOT NULL,
--     event JSONB NOT NULL,
--     created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
--     );
--     """) Row0

runQuery :: forall a. PG.Pool -> (PG.Connection -> PG a) -> Aff (Either PG.PGError a)
runQuery pool query = runExceptT do
  withConnection pool $ \conn -> do
    withTransaction conn $ query conn

addEvent :: String -> Json -> PG.Pool -> Aff (Either PG.PGError Unit)
addEvent app event pool = runQuery pool \conn -> do
  PG.execute conn (PG.Query """
    INSERT INTO events (app, event)
    VALUES ($1, $2);
  """) (app /\ event)

syncAll :: String -> (Array Json) -> PG.Pool -> Aff (Either PG.PGError Unit)
syncAll app events pool = runQuery pool \conn -> do
  PG.execute conn (PG.Query """
    DELETE FROM events
    WHERE app = ($1);
  """) (Row1 app)
  void $ for events \event -> do
    PG.execute conn (PG.Query """
      INSERT INTO events (app, event)
      VALUES ($1, $2);
    """) (app /\ event)

retrieveEvents :: String -> PG.Pool -> Aff (Either PG.PGError (Array Json))
retrieveEvents app pool = runQuery pool \conn -> do
  rows <- PG.query conn (PG.Query """
    SELECT event FROM events
    WHERE app = ($1)
    ORDER BY id desc;
    """) (Row1 app)
  pure $ map (\(Row1 json) -> json) rows

connectionMsg :: PG.PoolConfiguration -> String
connectionMsg poolConfig = "Connected to database " <> db <> " at " <> hostAndPort
  where db = poolConfig.database
        host = fromMaybe "" poolConfig.host
        port = fromMaybe "" $ show <$> poolConfig.port
        hostAndPort = host <> ":" <> port

getDB :: String -> Effect (Either String PG.Pool)
getDB dbUri = case fromURI dbUri of
  (Left err) -> pure (Left err)
  (Right poolConfig) -> do
    pool <- createConnectionPool poolConfig
    Console.log $ connectionMsg poolConfig
    pure $ Right pool
