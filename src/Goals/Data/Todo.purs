module Goals.Data.Todo where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.DateTime (DateTime)
import Data.Symbol (SProxy(..))
import Data.Maybe (Maybe(..), isJust)

import Utils.Lens (type (:->))
import Utils.Lens as L

newtype Todo = Todo {
  name :: String
, due :: DateTime
, comments :: String
, completionDate :: Maybe DateTime
}

derive instance newtypeGoal :: Newtype Todo _

_name :: Todo :-> String
_name = L.newtypeProp (SProxy :: SProxy "name")

_due :: Todo :-> DateTime
_due = L.newtypeProp (SProxy :: SProxy "due")

_comments :: Todo :-> String
_comments = L.newtypeProp (SProxy :: SProxy "comments")

_completionDate :: Todo :-> Maybe DateTime
_completionDate = L.newtypeProp (SProxy :: SProxy "completionDate")

markAsDone :: DateTime -> Todo -> Todo
markAsDone time = L.set _completionDate (Just time)

isDone :: Todo -> Boolean
isDone = L.view _completionDate >>> isJust

newTodo :: String -> DateTime -> String -> Todo
newTodo name due comments = wrap {name, due, comments, completionDate: Nothing}
