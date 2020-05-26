module Goals.Data.Todo where

import Prelude
import Data.Newtype (class Newtype, wrap)
import Data.DateTime (DateTime)
import Data.Symbol (SProxy(..))
import Utils.Lens (type (:->))
import Utils.Lens as L

newtype Todo = Todo {
  name :: String
, due :: DateTime
, comments :: String
}

derive instance newtypeGoal :: Newtype Todo _

_name :: Todo :-> String
_name = L._newtype >>> L.prop (SProxy :: SProxy "name")

_due :: Todo :-> DateTime
_due = L._newtype >>> L.prop (SProxy :: SProxy "due")

_comments :: Todo :-> String
_comments = L._newtype >>> L.prop (SProxy :: SProxy "comments")

newTodo :: String -> DateTime -> String -> Todo
newTodo name due comments = wrap {name, due, comments}
