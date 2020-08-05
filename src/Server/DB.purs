module Server.DB where

import Prelude
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for)
import Effect.Aff (Aff)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Database.PostgreSQL.PG as PG
import Database.PostgreSQL.Row (Row0(Row0), Row1(Row1), Row2(Row2))
import JohnCowie.PostgreSQL (runQuery, toPGError)
import Server.Domain (AppName, EventId, NewUser, User, UserId)
import Control.Monad.Except.Trans (ExceptT(..))

-- type PG a
--   = ExceptT PG.PGError Aff a

addEvent ::
  forall a.
  (EncodeJson a) =>
  UserId ->
  AppName ->
  a ->
  PG.Pool ->
  Aff (Either PG.PGError EventId)
addEvent userId app event pool =
  runQuery pool \conn -> do
    rows <-
      PG.query conn
        ( PG.Query
            """
    INSERT INTO EVENTS (id, app, event, user_id)
    VALUES (
	      (SELECT COALESCE(MAX(id), 0) + 1 FROM events)
      , $1
      , $2
      , $3
    )
    RETURNING ID;
  """
        )
        (unwrap app /\ encodeJson event /\ unwrap userId)
    pure $ fromMaybe (wrap 0) $ head $ map (\(Row1 id) -> wrap id) rows

retrieveEvents ::
  forall a.
  (DecodeJson a) =>
  UserId ->
  AppName ->
  Maybe EventId ->
  PG.Pool ->
  Aff (Either PG.PGError (Array (Tuple EventId a)))
retrieveEvents userId app eventIdM pool =
  runQuery pool \conn -> do
    rows <-
      PG.query conn
        ( PG.Query
            """
    SELECT id, event FROM events
    WHERE app = $1
    AND user_id = $2
    AND id > $3
    ORDER BY id desc;
    """
        )
        (unwrap app /\ unwrap userId /\ maybe 0 unwrap eventIdM)
    ExceptT $ pure $ for rows
      $ \(Row2 id eventJson) -> do
          event <- lmap toPGError $ decodeJson eventJson
          pure $ (wrap id /\ event)

retrieveLatestSnapshot ::
  forall a.
  (DecodeJson a) =>
  UserId ->
  AppName ->
  PG.Pool ->
  Aff (Either PG.PGError (Maybe (Tuple a EventId)))
retrieveLatestSnapshot userId app pool =
  runQuery pool \conn -> do
    rows <-
      PG.query conn
        ( PG.Query
            """
    SELECT snapshot, up_to_event
    FROM snapshots
    WHERE app = $1
    AND user_id = $2
    ORDER BY up_to_event DESC
    LIMIT 1;
  """
        )
        (unwrap app /\ unwrap userId)
    parsedRows <-
      ExceptT $ pure $ for rows
        $ \(Row2 snapshotJson upToEvent) -> do
            snapshot <- lmap toPGError $ decodeJson snapshotJson
            pure $ (snapshot /\ wrap upToEvent)
    pure $ head $ parsedRows

insertSnapshot ::
  forall a.
  (EncodeJson a) =>
  UserId ->
  AppName ->
  a ->
  EventId ->
  PG.Pool ->
  Aff (Either PG.PGError Unit)
insertSnapshot userId app snapshot upToEvent pool =
  runQuery pool \conn -> do
    PG.execute conn
      ( PG.Query
          """
    DELETE FROM snapshots
    WHERE app = $1 AND user_id = $2;
  """
      )
      (unwrap app /\ unwrap userId)
    PG.execute conn
      ( PG.Query
          """
    INSERT INTO snapshots (id, app, user_id, snapshot, up_to_event)
    VALUES (
      (select coalesce(max(id), 0) + 1 from snapshots)
    , $1
    , $2
    , $3
    , $4
    );
  """
      )
      (unwrap app /\ unwrap userId /\ encodeJson snapshot /\ unwrap upToEvent)

insertState ::
  forall a.
  (EncodeJson a) =>
  UserId ->
  AppName ->
  a ->
  PG.Pool ->
  Aff (Either PG.PGError Unit)
insertState userId app state db =
  runQuery db \conn -> do
    PG.execute conn
      ( PG.Query
          """
    DELETE FROM states
    WHERE app = $1 AND user_id = $2;
  """
      )
      (unwrap app /\ unwrap userId)
    PG.execute conn
      ( PG.Query
          """
    INSERT INTO states (app, user_id, state)
    VALUES (
      $1
    , $2
    , $3
    )
  """
      )
      (unwrap app /\ unwrap userId /\ encodeJson state)

retrieveState ::
  forall a.
  (DecodeJson a) =>
  UserId ->
  AppName ->
  PG.Pool ->
  Aff (Either PG.PGError (Maybe a))
retrieveState userId app db =
  runQuery db \conn -> do
    vM <-
      PG.scalar conn
        ( PG.Query
            """
    SELECT state
    FROM states
    WHERE app = $1
    AND user_id = $2
    LIMIT 1;
  """
        )
        (unwrap app /\ unwrap userId)
    case vM of
      (Just json) -> ExceptT $ pure $ lmap toPGError $ decodeJson json
      Nothing -> pure Nothing

upsertUser :: NewUser -> PG.Pool -> Aff (Either PG.PGError UserId)
upsertUser user pool =
  runQuery pool \conn -> do
    rows <-
      PG.query conn
        ( PG.Query
            """
    INSERT INTO users (third_party, third_party_id, name) VALUES ($1, $2, $3)
    ON CONFLICT ON CONSTRAINT users_third_party_third_party_id_key
    DO UPDATE SET name = $3
    RETURNING id;
  """
        )
        (show user.thirdParty /\ user.thirdPartyId /\ user.name)
    case rows of
      [ (Row1 id) ] -> pure $ wrap id
      _ -> ExceptT $ pure $ Left $ PG.ConversionError "No ID returned"

retrieveUsers :: PG.Pool -> Aff (Either PG.PGError (Array User))
retrieveUsers pool =
  runQuery pool \conn -> do
    rows <-
      PG.query conn
        ( PG.Query
            """
    SELECT id, name FROM users;
  """
        )
        Row0
    pure $ map (\(Row2 id name) -> { id: wrap id, name }) rows
