module Dunbar.App where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Spork.App as App
import Spork.Html as H
import Spork.Interpreter (merge, basicEffect)
import Utils.Spork.TimerSubscription (runSubscriptions, tickSub, Sub)
import Utils.Lens as L

import Dunbar.Friend (Friend)
import Dunbar.Friend as Friend
import Dunbar.State (Friendships)
import Dunbar.State as State

type Model = {
  friendships :: Friendships
}

data Msg = Tick Instant

init :: App.Transition Effect Model Msg
init = App.purely {friendships}
  where friendships = State.processEvent (State.addFriendEvent "Emma" "Bridger") $
                      State.processEvent (State.addFriendEvent "Katherine" "Woods") $
                      State.empty

renderFriendRow :: forall id. (Show id) => Tuple id Friend -> H.Html Msg
renderFriendRow (Tuple id friend) = H.div [] [H.text $ show $ L.view Friend._name friend]

renderFriendsList :: Model -> H.Html Msg
renderFriendsList model = H.div [] $ map renderFriendRow $ State.friendList model.friendships

render :: Model -> H.Html Msg
render model = H.div [] [ H.h3 [] [H.text "My Friends"]
                        , renderFriendsList model]

update :: Model → Msg → App.Transition Effect Model Msg
update model (Tick instant) = App.purely model

subs :: Model -> App.Batch Sub Msg
subs model = App.lift (tickSub Tick)

app :: App.App Effect Sub Model Msg
app = {
  render
, update
, subs
, init
}

runApp :: Effect Unit
runApp = do
  inst <- App.makeWithSelector (basicEffect `merge` runSubscriptions) app "#app"
  inst.run
