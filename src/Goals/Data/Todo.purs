module Goals.Data.Todo where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.DateTime (DateTime)
import Data.Symbol (SProxy(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)

import Utils.JsonDateTime (JsonDateTime)
import Utils.JsonNewtype (encodeNewtype, decodeNewtype)
import Utils.Lens (type (:->))
import Utils.Lens as L

newtype Todo = Todo {
  name :: String
, due :: JsonDateTime
, comments :: String
, completionDate :: Maybe (JsonDateTime)
}

derive instance newtypeGoal :: Newtype Todo _

instance decodeJsonTodo :: DecodeJson Todo where
  decodeJson = decodeNewtype "Todo"

instance encodeJsonTodo :: EncodeJson Todo where
  encodeJson = encodeNewtype

_name :: Todo :-> String
_name = L.newtypeProp (SProxy :: SProxy "name")

_due :: Todo :-> DateTime
_due = L.newtypeProp (SProxy :: SProxy "due") >>> L._newtype

_comments :: Todo :-> String
_comments = L.newtypeProp (SProxy :: SProxy "comments")

_completionDate :: Todo :-> Maybe DateTime
_completionDate = L.newtypeProp (SProxy :: SProxy "completionDate") >>> L.isoToLens (L.liftIso L.newtypeIso)

markAsDone :: DateTime -> Todo -> Todo
markAsDone time = L.set _completionDate (Just time)

isDone :: Todo -> Boolean
isDone = L.view _completionDate >>> isJust

newTodo :: String -> DateTime -> String -> Todo
newTodo name due comments = wrap {name, due: wrap due, comments, completionDate: Nothing}
