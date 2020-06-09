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


migrationData :: Array (Migration Int String)
migrationData = [
  createEventsTable 1
]

validateMigrations :: Array (Migration Int String) -> Either String (Array (Migration Int String))
validateMigrations = Right

migrationStore :: forall m. (Monad m) => MigrationStore m Int String
migrationStore = {loadMigrations: pure $ validateMigrations migrationData}
