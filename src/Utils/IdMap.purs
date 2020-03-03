module Utils.IdMap where

import Prelude
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Foldable (maximum)
import Data.Tuple (Tuple, fst)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

type IdMap v = Map Id v

newtype Id = Id Int
derive instance newtypeId :: Newtype Id _
derive instance eqId :: Eq Id
derive instance ordId :: Ord Id

instance decodeJsonId :: DecodeJson Id where
  decodeJson json = wrap <$> decodeJson json

instance encodeJsonId :: EncodeJson Id where
  encodeJson = encodeJson <<< unwrap

new :: forall v. IdMap v
new = M.empty

createId :: forall v. IdMap v -> Id
createId = wrap <<< (+) 1 <<< fromMaybe (-1) <<< maximum <<< (map unwrap) <<< keys

add :: forall v. v -> IdMap v -> IdMap v
add v m = M.insert (createId m) v m

update :: forall v. Id -> (v -> v) -> IdMap v -> IdMap v
update id f = M.update (Just <<< f) id

delete :: forall v. Id -> IdMap v -> IdMap v
delete id = M.update (const Nothing) id

toList :: forall v. IdMap v -> Array (Tuple Id v)
toList = M.toUnfoldable

keys :: forall v. IdMap v -> Array Id
keys = (map fst) <<< toList
