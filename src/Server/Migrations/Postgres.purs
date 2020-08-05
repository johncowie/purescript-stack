module Server.Migrations.Postgres where

import Prelude
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Server.Migrations (Executor, VersionStore, Migration)
import Server.DB (runQuery, showDBError)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Array (head)
import Effect.Console as Console
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Database.PostgreSQL.PG as PG
import Database.PostgreSQL.Row (Row0(Row0), Row1(Row1), Row3(Row3))

executeMigration :: forall id. (Show id) => PG.Pool -> id -> String -> Aff (Either String Unit)
executeMigration pool id query =
  runExceptT do
    liftEffect $ Console.log $ "Running migration: " <> show id
    ExceptT $ showDBError
      <$> runQuery pool \conn -> do
          PG.execute conn (PG.Query query) Row0

createTableQuery :: String
createTableQuery =
  """
  CREATE TABLE IF NOT EXISTS _migrations
  ( id INTEGER PRIMARY KEY
  , description VARCHAR
  , type VARCHAR NOT NULL
  , created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
"""

retrieveVersionQuery :: String
retrieveVersionQuery =
  """
  SELECT id FROM _migrations
  ORDER BY created desc
  LIMIT 1;
"""

updateVersionQuery :: String
updateVersionQuery =
  """
  INSERT INTO  _migrations (id, description, type) VALUES ($1, $2, $3);
"""

currentIntVersion :: PG.Pool -> Aff (Either String (Maybe Int))
currentIntVersion pool =
  runExceptT do
    ExceptT $ showDBError
      <$> runQuery pool \conn -> do
          PG.execute conn (PG.Query createTableQuery) Row0
    rows <-
      ExceptT $ showDBError
        <$> runQuery pool \conn -> do
            PG.query conn (PG.Query retrieveVersionQuery) Row0
    pure $ (\(Row1 id) -> id) <$> head rows

updateIntVersion :: forall a. PG.Pool -> Boolean -> Migration Int a -> Aff (Either String Unit)
updateIntVersion pool isUp migration =
  runExceptT do
    ExceptT $ showDBError
      <$> runQuery pool \conn -> do
          PG.execute conn (PG.Query updateVersionQuery) (Row3 migration.id migration.description migrationType)
  where
  migrationType = if isUp then "UP" else "DOWN"

executor :: forall id. (Show id) => PG.Pool -> Executor Aff id String
executor pool = { executeMigration: executeMigration pool }

intVersionStore :: forall a. PG.Pool -> VersionStore Aff Int a
intVersionStore pool =
  { currentVersion: currentIntVersion pool
  , updateVersion: updateIntVersion pool
  }
