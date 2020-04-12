module Utils.Components.Input where

import Prelude
import Data.Either (Either, either)
import Data.Map as M
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
  lens :: L.Lens' m String,
  placeholder :: String
}

-- abstract out message
stringInput :: forall model a.
               L.Lens' model Inputs
            -> String
            -> (String -> Either String a)
            -> String
            -> StringInput model a
stringInput _Inputs placeholder validator inputId = {
  validator: validator,
  inputLabel: "unused",
  lens: _Inputs >>> L._mapVal "" inputId,
  placeholder: placeholder,
  inputId: inputId -- TODO use this as basis of lens
}

-- TODO abstract out msg/model and move to utils
renderStringInput :: forall model msg a.
                     (L.Lens' model String -> String -> msg)
                  -> StringInput model a
                  -> model
                  -> H.Html msg
renderStringInput actionF input model =
  H.input [P.placeholder input.placeholder,
            P.type_ P.InputText,
            P.value (L.view input.lens model),
            E.onValueInput (E.always (actionF input.lens))]

clearInput :: forall model a. StringInput model a -> model -> model
clearInput input model = L.set input.lens "" model

parseStringInputUnsafe :: forall model a. StringInput model a -> model -> a
parseStringInputUnsafe input model = either unsafeThrow identity $ input.validator $ L.view input.lens model
