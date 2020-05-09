module Utils.Components.Input where

import Prelude
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Map as M
import Data.Tuple (Tuple(..))
import Data.Int as Int
import Data.Number as Number
import Data.DateTime (date)
import Data.Formatter.DateTime as F
import Data.Date (Date)
import Utils.Lens as L
import Utils.DateTime (dateToDateTime)
import Spork.Html as H
import Spork.Html.Properties as P
import Spork.Html.Events as E
import Effect.Exception.Unsafe (unsafeThrow)

type Inputs = M.Map String String

type ModelWithInputs r = {inputs :: Inputs | r}

type StringInput m a = {
  validator :: String -> Either String a,
  lens :: L.Lens' m String
}

-- does this already exist? i.e. encode/decode
class InputType a where
  parseInput :: String -> Either String a
  showInput :: a -> String

instance inputTypeInt :: InputType Int where
  parseInput s = case Int.fromString s of
    Nothing -> Left "Not a valid integer"
    (Just i) -> Right i
  showInput = show

instance inputTypeNumber :: InputType Number where
  parseInput = parseNumber
  showInput = show

instance inputTypeString :: InputType String where
  parseInput "" = (Left "Can't be empty")
  parseInput s = Right s
  showInput = show

instance inputTypeDate :: InputType Date where
  parseInput = parseDate
  showInput = showDate

instance inputTypeMaybe :: (InputType a) => InputType (Maybe a) where
  parseInput "" = Right Nothing
  parseInput s = Just <$> parseInput s
  showInput = maybe "" showInput

nonEmptyString :: String -> Either String String
nonEmptyString "" = Left "Can't be empty"
nonEmptyString s = Right s

showDate :: Date -> String
showDate = either unsafeThrow identity <<< F.formatDateTime "DD/MM/YYYY" <<< dateToDateTime

parseInt :: String -> Either String Int
parseInt s = case Int.fromString s of
  Nothing -> Left "Not a valid integer"
  (Just i) -> Right i

parseNumber :: String -> Either String Number
parseNumber s = case Number.fromString s of
  Nothing -> Left "Not a valid number"
  (Just i) -> Right i

parseDate :: String -> Either String Date
parseDate s = date <$> F.unformatDateTime "DD/MM/YYYY" s

-- way of getting value out of inputs, and way of setting it

stringInput :: forall model a.
               (InputType a)
            => L.Lens' model Inputs
            -> String
            -> StringInput model a
stringInput _inputs inputId = {
  validator: parseInput,
  lens: _inputs >>> L._mapVal "" inputId
}

nonEmptyStringInput :: forall model.
                       L.Lens' model Inputs
                    -> String
                    -> StringInput model String
nonEmptyStringInput _inputs = stringInput _inputs

intInput :: forall model.
            L.Lens' model Inputs
         -> String
         -> StringInput model Int
intInput _inputs = stringInput _inputs

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
