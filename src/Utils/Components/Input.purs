module Utils.Components.Input
( InputData
, Inputs
, StringInput
, class InputType
, parseInput
, showInput
, stringInput
, renderStringInput
, renderDropdown
, renderDropdown_
, clearInput
, setInput
, setInputFromVal
, parseStringInputUnsafe
)
where

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
import Data.Symbol (SProxy(..))
import Utils.Lens as L
import Utils.DateTime (dateToDateTime)
import Spork.Html as H
import Spork.Html.Properties as P
import Spork.Html.Events as E
import Effect.Exception.Unsafe (unsafeThrow)

type InputData = { rawValue :: String
                 , processedValue :: Either String String }

emptyInputData :: InputData
emptyInputData = { rawValue: ""
                 , processedValue: Right ""}

_inputDataVal :: L.Lens' InputData String
_inputDataVal = L.prop (SProxy :: SProxy "rawValue")

type Inputs = M.Map String InputData

newtype StringInput m a = StringInput {
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
stringInput _inputs inputId = StringInput {
  validator: parseInput,
  lens: _inputs >>> L._mapVal emptyInputData inputId >>> _inputDataVal
}

renderStringInput :: forall model msg a.
                     (L.Lens' model String -> String -> msg)
                  -> StringInput model a
                  -> String
                  -> model
                  -> H.Html msg
renderStringInput actionF (StringInput input) placeholder model =
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
renderDropdown actionF (StringInput input) options model =
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

setInput :: forall model a. String -> StringInput model a -> model -> model
setInput s (StringInput input) = L.set input.lens s

clearInput :: forall model a. StringInput model a -> model -> model
clearInput = setInput ""

setInputFromVal :: forall model a. (InputType a) => Maybe a -> StringInput model a -> model -> model
setInputFromVal val (StringInput input) model = L.set input.lens (maybe "" showInput val) model

parseStringInputUnsafe :: forall model a. StringInput model a -> model -> a
parseStringInputUnsafe (StringInput input) model = either unsafeThrow identity $ input.validator $ L.view input.lens model
