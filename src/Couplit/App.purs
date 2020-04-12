module Couplit.App where

import Prelude
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Spork.Html (Html)
import Spork.Html as H
import Spork.Html.Properties as P
import Spork.PureApp (PureApp)
import Spork.PureApp as PureApp
import Data.Maybe (Maybe(..))
import Data.Array (length, replicate)

import Utils.Url as Url

type Poem = {
  theme :: String
, lines :: Array String
, maxLines :: Int
}

type UI = {}

type Model = {
  poem :: Maybe Poem
}

data Action = DoSomething

update :: Model -> Action -> Model
update m a = m

renderPoemForm :: Model -> Poem -> Html Action
renderPoemForm m p = H.div [] [
  H.h3 [] [H.text p.theme],
  H.div [H.classes ["poem"]] $ map poemLine p.lines,
  H.div [] $ [lineInput],
  H.div [] $ replicate remainingLineCount remainingLine
] where remainingLineCount = p.maxLines - length p.lines
        poemLine s = H.div [H.classes ["poemLine"]] [H.text s]
        remainingLine = H.div [H.classes ["remainingLine"]] [H.text "another line"]
        lineInput = H.input [P.placeholder "next line",
                             P.type_ P.InputText,
                             P.value ""]

render :: Model -> Html Action
render m = case m.poem of
  (Just poem) -> renderPoemForm m poem
  Nothing -> H.text "No poem yet - probably should be a form here"

init :: String -> Model
init url = {
  poem: Just {
    theme: "Just do a shitty poem",
    lines: [ "Tiger tiger burning bright"
           , "Like a dildo in the night"],
    maxLines: 10
  }
}

app :: String -> PureApp Model Action
app url = { update, render, init: init url }

runApp :: Effect Unit
runApp = do
  url <- Url.getWindowUrl
  let queryParams = show $ Url.getQueryParams url
  void $ PureApp.makeWithSelector (app queryParams) "#app"

-- TODO
-- [ ] Abstract out input field utility
-- [ ] Wire up input field
-- [ ] Add submit button
-- [ ] Wire up submit button
-- [ ] Rejib app to support Action effects
-- [ ] Render form for new poem
-- [ ] Wire up form for new poem
-- [ ]
