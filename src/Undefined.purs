module Undefined
( undefined )
where

import Effect.Exception.Unsafe (unsafeThrow)

undefined :: forall a b. a -> b
undefined = unsafeThrow "undefined"
