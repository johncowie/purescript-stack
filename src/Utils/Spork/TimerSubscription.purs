module Utils.Spork.TimerSubscription
(runSubscriptions,
 tickSub,
 Sub
)
where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Data.DateTime.Instant (Instant) as Date
import Effect.Now (now) as Date
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..))
import Data.Foldable (traverse_)
import Effect.Timer as Timer

data Sub a = TickTime (Date.Instant -> a)
derive instance functorSub :: Functor Sub

tickSub :: forall a. (Date.Instant -> a) -> Sub a
tickSub = TickTime

runSubscriptions :: forall i. Interpreter Effect Sub i
runSubscriptions = Interpreter $ EventQueue.withAccumArray \queue -> do
    model <- Ref.new []

    let
        tick :: Effect Unit
        tick = do
            now <- Date.now
            Ref.read model >>= traverse_ case _ of
                TickTime k -> queue.push (k now)
            queue.run

        commit :: Array (Sub i) -> Effect Unit
        commit new = do
            old <- Ref.read model
            Ref.write new model
            case old, new of
                [], _ -> void $ Timer.setInterval 1000 tick
                _, _  -> pure unit
            pure unit
    pure commit
