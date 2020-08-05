-- module just for trying out bits of code in the console
module Experiment where

import Prelude
import Effect (Effect)
import Effect.Console as C
import Data.Tuple (Tuple(..))
import Data.Array as Array

main :: Effect Unit
main = C.logShow $ Array.sortWith sorter [ { a: 1, b: 2 }, { a: 3, b: 2 }, { a: 3, b: 1 } ]
  where
  sorter r = Tuple r.a r.b
