module Server.DB where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Either (Either)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (errorShow)
import Effect.Aff (Aff, runAff)
import Effect.Class (liftEffect)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson) as JSON

import Database.PostgreSQL.PG as PG
import Database.PostgreSQL.Row (Row1(Row1))

type PG a = ExceptT PG.PGError Aff a

withConnection :: forall a. PG.Pool -> (PG.Connection -> PG a) -> PG a
withConnection = PG.withConnection runExceptT

withTransaction :: forall a. PG.Connection -> PG a -> PG a
withTransaction = PG.withTransaction runExceptT

createConnectionPool :: String -> PG PG.Pool
createConnectionPool dbName = liftEffect $ PG.newPool
  ((PG.defaultPoolConfiguration dbName) { idleTimeoutMillis = Just 1000 })

-- createEventsTable :: PG.Connection -> PG Unit
-- createEventsTable conn = PG.execute conn (PG.Query """
--   CREATE TABLE events (
--     id SERIAL PRIMARY KEY,
--     app TEXT NOT NULL,
--     event JSONB NOT NULL,
--     created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
--     );
--     """) Row0

runQuery :: forall a. (PG.Connection -> PG a) -> Aff (Either PG.PGError a)
runQuery query = runExceptT do
  pool <- createConnectionPool "events_store"
  withConnection pool $ \conn -> do
    withTransaction conn $ query conn

addEvent :: String -> Json -> Aff (Either PG.PGError Unit)
addEvent app event = runQuery \conn -> do
  PG.execute conn (PG.Query """
    INSERT INTO events (app, event)
    VALUES ($1, $2);
  """) (app /\ event)

syncAll :: String -> (Array Json) -> Aff (Either PG.PGError Unit)
syncAll app events = runQuery \conn -> do
  PG.execute conn (PG.Query """
    DELETE FROM events
    WHERE app = ($1);
  """) (Row1 app)
  void $ for events \event -> do
    PG.execute conn (PG.Query """
      INSERT INTO events (app, event)
      VALUES ($1, $2);
    """) (app /\ event)

retrieveEvents :: String -> Aff (Either PG.PGError (Array Json))
retrieveEvents app = runQuery $ \conn -> do
  rows <- PG.query conn (PG.Query """
    SELECT event FROM events
    WHERE app = ($1)
    ORDER BY id desc;
    """) (Row1 app)
  pure $ map (\(Row1 json) -> json) rows

getDB :: Aff (Either PG.PGError PG.Pool)
getDB = runExceptT $ createConnectionPool "events_store"

addSomeExampleStuff :: PG.Connection -> PG Unit
addSomeExampleStuff conn = PG.execute conn (PG.Query """
  INSERT INTO events (app, event)
  VALUES ($1, $2);
  """) ("goals" /\ (JSON.encodeJson {hello: "world"}))

addSampleData :: PG Unit
addSampleData = do
  pool <- createConnectionPool "events_store"
  withConnection pool $ \conn -> do
    withTransaction conn $ do
      addSomeExampleStuff conn

main :: Effect Unit
main = do
  void $ runAff errorShow $ runExceptT addSampleData
