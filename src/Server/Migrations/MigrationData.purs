module Server.Migrations.MigrationData where

import Prelude
import Server.Migrations (MigrationStore, Migration)
import Data.Either (Either(..))

createEventsTable :: Int -> Migration Int String
createEventsTable id = {id, up, down, description}
  where up = """
          CREATE TABLE IF NOT EXISTS events (
            id SERIAL PRIMARY KEY,
            app TEXT NOT NULL,
            event JSONB NOT NULL,
            created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
          );
        """
        down = """
          DROP TABLE IF EXISTS events;
        """
        description = "Create events table"

createSnapshotsTable :: Int -> Migration Int String
createSnapshotsTable id = {id, up, down, description}
  where up = """
          CREATE TABLE IF NOT EXISTS snapshots (
            id INTEGER PRIMARY KEY,
            app TEXT NOT NULL,
            snapshot JSONB NOT NULL,
            up_to_event INTEGER NOT NULL,
            created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (up_to_event) REFERENCES events (id)
          );
        """
        down = """
          DROP TABLE IF EXISTS snapshots;
        """
        description = "Create snapshots table"


migrationData :: Array (Migration Int String)
migrationData = [
  createEventsTable 1
, createSnapshotsTable 2
]

validateMigrations :: Array (Migration Int String) -> Either String (Array (Migration Int String))
validateMigrations = Right

migrationStore :: forall m. (Monad m) => MigrationStore m Int String
migrationStore = {loadMigrations: pure $ validateMigrations migrationData}
