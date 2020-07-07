module Undefined
( undefined )
where

import Effect.Exception.Unsafe (unsafeThrow)

undefined :: forall a. a
undefined = unsafeThrow "undefined"
