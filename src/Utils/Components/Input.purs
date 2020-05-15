module Utils.Components.Input
( InputData
, Inputs
, StringInput
, InputSetter
, class InputType
, parseInput
, showInput
, newInputs
, stringInput
, renderStringInput
, renderDropdown
, renderDropdown_
, clearInput
, updateInput
, setInput
, setInputFromVal
, inputValue
, parseStringInputUnsafe
)
where

import Prelude
import Data.Either (Either(..), either, isLeft)
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

type InputData = { rawValue :: String
                 , processedValue :: Either String String }

data InputSetter model = InputSetter (L.Lens' model InputData) (String -> Either String String)

emptyInputData :: InputData
emptyInputData = { rawValue: ""
                 , processedValue: Right ""}

hasError :: InputData -> Boolean
hasError = isLeft <<<  _.processedValue

updateInputData :: (String -> Either String String) -> String -> InputData -> InputData
updateInputData validator s _inputData = { rawValue, processedValue }
  where rawValue = s
        processedValue = validator s

stringifyValidator :: forall a. (InputType a)
                             => (String -> Either String a)
                             -> (String -> Either String String)
stringifyValidator validator = \s -> showInput <$> validator s

type Inputs = M.Map String InputData

newInputs :: M.Map String InputData
newInputs = M.empty

newtype StringInput m a = StringInput {
  validator :: String -> Either String a,
  lens :: L.Lens' m InputData
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
  validator: parseInput, -- TODO remove?
  lens: _inputs >>> L._mapVal emptyInputData inputId
}

mkInputSetter :: forall model a.
                 (InputType a)
              => StringInput model a
              -> InputSetter model
mkInputSetter (StringInput input) = InputSetter input.lens (stringifyValidator input.validator)

renderStringInput :: forall model msg a.
                     (InputType a)
                  => (InputSetter model -> String -> msg)
                  -> StringInput model a
                  -> String
                  -> model
                  -> H.Html msg
renderStringInput actionF (StringInput input) placeholder model =
  H.input [ P.placeholder placeholder
          , P.type_ P.InputText
          , P.value $ inputData.rawValue
          , E.onValueInput (E.always (actionF (mkInputSetter (StringInput input))))
          , H.classes classes]
  where inputData = L.view input.lens model
        classes = if hasError inputData then ["invalid"] else []

-- TODO selectedVal should be of type A
renderOption :: forall msg a. (InputType a) => String -> Tuple String a -> H.Html msg
renderOption selectedVal (Tuple label val) =
  H.option [ P.value (showInput val)
           , H.prop "selected" (showInput val == selectedVal)]
           [H.text label]

renderDropdown :: forall model msg a.
                  (InputType a)
               => (InputSetter model -> String -> msg)
               -> StringInput model a
               -> Array (Tuple String a)
               -> model
               -> H.Html msg
renderDropdown actionF (StringInput input) options model =
  H.select [E.onValueChange (E.always (actionF (mkInputSetter (StringInput input))))] $ map (renderOption selected) options
  where selected = _.rawValue $ L.view input.lens model

renderDropdown_ :: forall model msg a.
                   (InputType a)
                => (InputSetter model -> String -> msg)
                -> StringInput model a
                -> Array a
                -> model
                -> H.Html msg
renderDropdown_ actionF input optionVals model =
  renderDropdown actionF input options model
  where options = map (\v -> Tuple (showInput v) v) optionVals

updateInput :: forall model. InputSetter model -> String -> model -> model
updateInput (InputSetter l v) s = L.over l (updateInputData v s)

setInput :: forall model a. (InputType a) => String -> StringInput model a -> model -> model
setInput s (StringInput input) = L.over input.lens (updateInputData (stringifyValidator input.validator) s)

clearInput :: forall model a. (InputType a) => StringInput model a -> model -> model
clearInput (StringInput input) = L.set input.lens emptyInputData

setInputFromVal :: forall model a. (InputType a) => Maybe a -> StringInput model a -> model -> model
setInputFromVal val = setInput (maybe "" showInput val)

inputValue :: forall model a.StringInput model a -> model -> String
inputValue (StringInput input) = L.view input.lens >>> _.rawValue

parseStringInputUnsafe :: forall model a. StringInput model a -> model -> a
parseStringInputUnsafe (StringInput input) model = either unsafeThrow identity $ input.validator $ _.rawValue $ L.view input.lens model
