module Utils.Components.Lib where

import Prelude
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Data.Maybe (Maybe)
import Spork.PureApp (PureApp)
import Spork.PureApp as PureApp
import Spork.Html as H
import Utils.Lens as L
import Utils.Components.Input as I
-- import Effect.Exception.Unsafe (unsafeThrow)

type Model = {
  inputs :: I.Inputs
}

_inputs :: L.Lens' Model I.Inputs
_inputs = L.prop (SProxy :: SProxy "inputs")


-- TODO remove lens from this? (Maybe have a custom InputUpdater type
data Action = UpdateInput (I.InputSetter Model) String

stringInput1 :: I.StringInput Model (Maybe String)
stringInput1 = I.stringInput _inputs "input1"

stringInput2 :: I.StringInput Model Int
stringInput2 = I.stringInput _inputs "input2"

blankInputExample :: Model -> H.Html Action
blankInputExample = I.renderStringInput UpdateInput stringInput1 "fillMeIn"

invalidInputExample :: Model -> H.Html Action
invalidInputExample = I.renderStringInput UpdateInput stringInput2 "fillMeIn"

componentExample :: String -> H.Html Action -> H.Html Action
componentExample title component = H.div [] [
  H.h3 [] [H.text title]
, component
]

render :: Model -> H.Html Action
render m = H.div [] [
  componentExample "Blank String Input" (blankInputExample m)
, componentExample "Invalid String Input" (invalidInputExample m)
]

update :: Model -> Action -> Model
update m (UpdateInput setter v) = I.updateInput setter v m

init :: Model
init = I.setInput "blah" stringInput2 $ {inputs}
  where inputs = I.newInputs

app :: PureApp Model Action
app = {
  update,
  render,
  init
}

main ∷ Effect Unit
main = void $ PureApp.makeWithSelector app "#app"
