module Utils.Components.Input
( InputData
, Inputs
, StringInput
, Input
, InputSetter
, class InputType
, parseInput
, showInput
, newInputs
, stringInput
, stringInput_
, renderStringInput
, renderTextArea
, renderDropdown
, renderDropdown_
, clearInput
, updateInput
, setInput
, setInputFromVal
, inputValue
, parseStringInput
)
where

import Prelude
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Map as M
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Int as Int
import Data.Number as Number
import Data.DateTime (date)
import Data.Formatter.DateTime as F
import Data.Date (Date)
import Data.Symbol (SProxy(..))
import Utils.Lens as L
import Utils.Lens (type (:->))
import Utils.DateTime (dateToDateTime)
import Spork.Html as H
import Spork.Html.Properties as P
import Spork.Html.Events as E
import Effect.Exception.Unsafe (unsafeThrow)

type InputData = { rawValue :: String
                 , error :: Maybe String }

data InputSetter model = InputSetter (model :-> String)

emptyInputData :: InputData
emptyInputData = { rawValue: ""
                 , error: Nothing}

hasError :: InputData -> Boolean
hasError = isJust <<<  _.error

stringifyValidator :: forall a. (InputType a)
                             => (String -> Either String a)
                             -> (String -> Either String String)
stringifyValidator validator = \s -> showInput <$> validator s

type Inputs = M.Map String InputData

newInputs :: Inputs
newInputs = M.empty

newtype Input m a = Input {
  validator :: String -> Either String a,
  lens :: m :-> InputData
}
type StringInput m a = Input m a -- todo remove me

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
  showInput = identity

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
stringInput _inputs inputId = Input {
  validator: parseInput,
  lens: _inputs >>> L._mapVal emptyInputData inputId
}

stringInput_ :: forall model a.
                (InputType a)
             => L.Lens' model Inputs
             -> String
             -> a
             -> StringInput model a
stringInput_ _inputs inputId initialVal = Input {
  validator: parseInput, -- TODO remove?
  lens: _inputs >>> L._mapVal startData inputId
} where startData = emptyInputData {rawValue = showInput initialVal}


mkInputSetter :: forall model a.
                 (InputType a)
              => StringInput model a
              -> InputSetter model
mkInputSetter (Input input) = InputSetter (input.lens >>> _validatedValue input.validator)

_validatedValue :: forall a. (InputType a) => (String -> Either String a) -> InputData :-> String
_validatedValue validator = L.lens getter setter
  where getter = _.rawValue
        setter m v = case validator v of
          (Left err) -> {rawValue: v, error: Just err}
          (Right _) -> {rawValue: v, error: Nothing}

_rawValue :: InputData :-> String
_rawValue = L.prop (SProxy :: SProxy "rawValue")

renderStringInput :: forall model msg a.
                     (InputType a)
                  => (InputSetter model -> String -> msg)
                  -> StringInput model a
                  -> String
                  -> model
                  -> H.Html msg
renderStringInput actionF (Input input) placeholder model =
  H.input [ P.placeholder placeholder
          , P.type_ P.InputText
          , P.value $ inputData.rawValue
          , E.onValueInput (E.always (actionF (mkInputSetter (Input input))))
          , H.classes classes]
  where inputData = L.view input.lens model
        classes = if hasError inputData then ["invalid"] else []

renderTextArea :: forall model msg a.
                  (InputType a)
               => (InputSetter model -> String -> msg)
               -> StringInput model a
               -> String
               -> model
               -> H.Html msg
renderTextArea actionF (Input input) placeholder model =
  H.textarea [ P.placeholder placeholder
             , P.value $ inputData.rawValue
             , E.onValueInput (E.always (actionF (mkInputSetter (Input input))))
             , H.classes classes]
  where inputData = L.view input.lens model
        classes = if hasError inputData then ["invalid"] else []


renderOption :: forall msg a. (Eq a) => (InputType a) => Maybe a -> Tuple String a -> H.Html msg
renderOption selectedVal (Tuple label val) =
  H.option [ P.value (showInput val)
           , H.prop "selected" (Just val == selectedVal)]
           [H.text label]

renderDropdown :: forall model msg a.
                  (InputType a)
               => (Eq a)
               => (InputSetter model -> String -> msg)
               -> StringInput model a
               -> Array (Tuple String a)
               -> model
               -> H.Html msg
renderDropdown actionF (Input input) options model =
  H.select [E.onValueChange (E.always (actionF (mkInputSetter (Input input))))] $ map (renderOption selected) options
  where selected = either (const Nothing) Just $ input.validator currentValue
        currentValue = _.rawValue $ L.view input.lens model

renderDropdown_ :: forall model msg a.
                   (InputType a)
                => (Eq a)
                => (InputSetter model -> String -> msg)
                -> StringInput model a
                -> Array a
                -> model
                -> H.Html msg
renderDropdown_ actionF input optionVals model =
  renderDropdown actionF input options model
  where options = map (\v -> Tuple (showInput v) v) optionVals

updateInput :: forall model. InputSetter model -> String -> model -> model
updateInput (InputSetter l) s = L.set l s

setInput :: forall model a. (InputType a) => String -> StringInput model a -> model -> model
setInput s (Input input) = L.set (input.lens >>> _rawValue) s

clearInput :: forall model a. (InputType a) => StringInput model a -> model -> model
clearInput (Input input) = L.set input.lens emptyInputData

setInputFromVal :: forall model a. (InputType a) => Maybe a -> StringInput model a -> model -> model
setInputFromVal val = setInput (maybe "" showInput val)

inputValue :: forall model a.StringInput model a -> model -> String
inputValue (Input input) = L.view input.lens >>> _.rawValue

readInput :: forall model v. (InputType v) => Input model v -> model -> Either (String /\ model) (v /\ model)
readInput (Input input) m =
  case input.validator val of
    (Left err) -> Left (err /\ m)
    (Right v) -> Right (v /\ clearInput (Input input) m)
  where val = inputValue (Input input) m
--
-- joinInputVals :: forall model a b.
--                     (model -> (Either String a) /\ model)
--                  -> (model -> (Either String b) /\ model)
--                  -> (model -> (Either String (a /\ b)) model)
-- joinInputVals = unsafeThrow "implement me"

parseStringInput :: forall model a. StringInput model a -> model -> Either String a
parseStringInput (Input input) model = input.validator $ _.rawValue $ L.view input.lens model
