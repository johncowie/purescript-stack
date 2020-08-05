module Utils.Async where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Timer as Timer
import Data.Either (Either(..))

async :: Effect ~> Aff
async e =
  makeAff \handler -> do
    void $ Timer.setTimeout 0
      $ do
          msg <- e
          handler (Right msg)
    pure mempty
