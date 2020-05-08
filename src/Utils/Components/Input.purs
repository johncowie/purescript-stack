module Utils.Components.Input where

import Prelude
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Map as M
import Data.Tuple (Tuple(..))
import Data.Int as Int
import Utils.Lens as L
import Spork.Html as H
import Spork.Html.Properties as P
import Spork.Html.Events as E
import Effect.Exception.Unsafe (unsafeThrow)

type Inputs = M.Map String String

type StringInput m a = {
  validator :: String -> Either String a,
  inputLabel :: String,
  inputId :: String,
  lens :: L.Lens' m String
}

nonEmptyString :: String -> Either String String
nonEmptyString "" = Left "Can't be empty"
nonEmptyString s = Right s

parseInt :: String -> Either String Int
parseInt s = case Int.fromString s of
  Nothing -> Left "Not a valid integer"
  (Just i) -> Right i

stringInput :: forall model a.
               L.Lens' model Inputs
            -> (String -> Either String a)
            -> String
            -> StringInput model a
stringInput _inputs validator inputId = {
  validator: validator,
  inputLabel: "unused",
  lens: _inputs >>> L._mapVal "" inputId,
  inputId: inputId -- TODO use this as basis of lens
}

nonEmptyStringInput :: forall model.
                       L.Lens' model Inputs
                    -> String
                    -> StringInput model String
nonEmptyStringInput _inputs = stringInput _inputs nonEmptyString

intInput :: forall model.
            L.Lens' model Inputs
         -> String
         -> StringInput model Int
intInput _inputs = stringInput _inputs parseInt 

renderStringInput :: forall model msg a.
                     (L.Lens' model String -> String -> msg)
                  -> StringInput model a
                  -> String
                  -> model
                  -> H.Html msg
renderStringInput actionF input placeholder model =
  H.input [ P.placeholder placeholder
          , P.type_ P.InputText
          , P.value (L.view input.lens model)
          , E.onValueInput (E.always (actionF input.lens))]


renderOption :: forall msg a. (Show a) => String -> Tuple String a -> H.Html msg
renderOption selectedVal (Tuple label val) =
  H.option [ P.value (show val)
           , H.prop "selected" (show val == selectedVal)]
           [H.text label]

renderDropdown :: forall model msg a.
                  (Show a)
               => (L.Lens' model String -> String -> msg)
               -> StringInput model a
               -> Array (Tuple String a)
               -> model
               -> H.Html msg
renderDropdown actionF input options model =
  H.select [E.onValueChange (E.always (actionF input.lens))] $ map (renderOption selected) options
  where selected = L.view input.lens model

renderDropdown_ :: forall model msg a.
                   (Show a)
                => (L.Lens' model String -> String -> msg)
                -> StringInput model a
                -> Array a
                -> model
                -> H.Html msg
renderDropdown_ actionF input optionVals model =
  renderDropdown actionF input options model
  where options = map (\v -> Tuple (show v) v) optionVals


clearInput :: forall model a. StringInput model a -> model -> model
clearInput input model = L.set input.lens "" model

setInput :: forall model a. (Show a) => Maybe a -> StringInput model a -> model -> model
setInput val input model = L.set input.lens s model
  where s = maybe "" show val

parseStringInputUnsafe :: forall model a. StringInput model a -> model -> a
parseStringInputUnsafe input model = either unsafeThrow identity $ input.validator $ L.view input.lens model
