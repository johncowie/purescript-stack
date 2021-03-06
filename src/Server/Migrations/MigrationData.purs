module Server.Migrations.MigrationData where

import Prelude
import JohnCowie.Migrations (MigrationStore, Migration, revert)
import Data.Either (Either(..))

createEventsTable :: Int -> Migration Int String
createEventsTable id = { id, up, down, description }
  where
  up =
    """
          CREATE TABLE IF NOT EXISTS events (
            id SERIAL PRIMARY KEY,
            app TEXT NOT NULL,
            event JSONB NOT NULL,
            created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
          );
        """

  down =
    """
          DROP TABLE IF EXISTS events;
        """

  description = "Create events table"

createSnapshotsTable :: Int -> Migration Int String
createSnapshotsTable id = { id, up, down, description }
  where
  up =
    """
          CREATE TABLE IF NOT EXISTS snapshots (
            id INTEGER PRIMARY KEY,
            app TEXT NOT NULL,
            snapshot JSONB NOT NULL,
            up_to_event INTEGER NOT NULL,
            created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (up_to_event) REFERENCES events (id)
          );
        """

  down =
    """
          DROP TABLE IF EXISTS snapshots;
        """

  description = "Create snapshots table"

createStatesTable :: Int -> Migration Int String
createStatesTable id = { id, up, down, description }
  where
  up =
    """
          CREATE TABLE IF NOT EXISTS states (
            id SERIAL PRIMARY KEY,
            user_id INTEGER NOT NULL REFERENCES users(id),
            app VARCHAR NOT NULL,
            state JSONB NOT NULL,
            created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
          );
        """

  down =
    """
          DROP TABLE IF EXISTS states;
        """

  description = "Create states table"

createUsersTable :: Int -> Migration Int String
createUsersTable id = { id, up, down, description }
  where
  up =
    """
              CREATE TABLE IF NOT EXISTS users (
                id SERIAL PRIMARY KEY
              , third_party VARCHAR NOT NULL
              , third_party_id VARCHAR NOT NULL
              , name VARCHAR
              , UNIQUE(third_party, third_party_id)
              );
             """

  down =
    """
          DROP TABLE IF EXISTS users;
        """

  description = """Create users table"""

createTokensTable :: Int -> Migration Int String
createTokensTable id = { id, up, down, description }
  where
  up =
    """
             CREATE TABLE IF NOT EXISTS tokens (
               user_id INTEGER PRIMARY KEY REFERENCES users(id)
             , token VARCHAR NOT NULL
             );
             """

  down =
    """
                 DROP TABLE IF EXISTS tokens;
               """

  description = "Create tokens table"

addUserIdToEvents :: Int -> Migration Int String
addUserIdToEvents id = { id, up, down, description }
  where
  up =
    """
             ALTER TABLE events
             ADD COLUMN user_id INTEGER;

             UPDATE events
             SET user_id = 1;

             ALTER TABLE events
             ALTER COLUMN user_id SET NOT NULL,
             ADD CONSTRAINT user_id_fk FOREIGN KEY (user_id) REFERENCES users(id);
             """

  down =
    """
               ALTER TABLE events
               DROP COLUMN user_id;
               """

  description = "Add user Id column to events table"

addUserIdToSnapshots :: Int -> Migration Int String
addUserIdToSnapshots id = { id, up, down, description }
  where
  up =
    """
             ALTER TABLE snapshots
             ADD COLUMN user_id INTEGER;

             UPDATE snapshots
             SET user_id = 1;

             ALTER TABLE snapshots
             ALTER COLUMN user_id SET NOT NULL,
             ADD CONSTRAINT snapshots_user_id_fk FOREIGN KEY (user_id) REFERENCES users(id);
             """

  down =
    """
               ALTER TABLE snapshots
               DROP COLUMN user_id;
               """

  description = "Add user id column to snapshots table"

migrationData :: Array (Migration Int String)
migrationData =
  [ createEventsTable 1
  , createSnapshotsTable 2
  , createUsersTable 3
  , createTokensTable 4
  , revert (createTokensTable 4) 5
  , addUserIdToEvents 6
  , addUserIdToSnapshots 7
  , createStatesTable 8
  ]

validateMigrations :: Array (Migration Int String) -> Either String (Array (Migration Int String))
validateMigrations = Right -- TODO?

migrationStore :: forall m. (Monad m) => MigrationStore m Int String
migrationStore = { loadMigrations: pure $ validateMigrations migrationData }
