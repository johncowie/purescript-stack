module CustomPrelude
( module Prelude
, undefined
, module Data.Maybe
, module Data.Either
)
where

import Prelude
import Effect.Exception.Unsafe (unsafeThrow)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

undefined :: forall a. a
undefined = unsafeThrow "undefined"
