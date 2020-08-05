module Utils.Spork.TimerSubscription
  ( runTicker
  , tickSub
  , Sub
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.DateTime.Instant (Instant) as Date
import Data.Time.Duration (Seconds)
import Effect.Now (now) as Date
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..))
import Data.Foldable (traverse_)
import Effect.Timer as Timer
import Data.Int (round)

data Sub a
  = TickTime (Date.Instant -> a)

derive instance functorSub :: Functor Sub

tickSub :: forall a. (Date.Instant -> a) -> Sub a
tickSub = TickTime

runTicker :: forall i. (Maybe Seconds) -> Interpreter Effect Sub i
runTicker Nothing =
  Interpreter
    $ EventQueue.withAccumArray \queue -> do
        pure (const (pure unit))

runTicker (Just secs) =
  Interpreter
    $ EventQueue.withAccumArray \queue -> do
        model <- Ref.new []
        let
          tick :: Effect Unit
          tick = do
            now <- Date.now
            Ref.read model
              >>= traverse_ case _ of
                  TickTime k -> queue.push (k now)
            queue.run

          commit :: Array (Sub i) -> Effect Unit
          commit new = do
            old <- Ref.read model
            Ref.write new model
            case old, new of
              [], _ -> void $ Timer.setInterval (round (unwrap secs) * 1000) tick
              _, _ -> pure unit
            pure unit
        pure commit
