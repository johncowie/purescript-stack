module JohnCowie.HTTPure.QueryParams
( class GDecodeQueryParams
, class ParseQueryParam
, QueryParamKey
, nonExistantKeyError
, gDecodeQueryParams
, parseQueryParam
, parseQueryParams
, parseNewtype )
where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Int as Int
import Data.Newtype (class Newtype, wrap, unwrap)

import Foreign.Object (Object)
import Foreign.Object as FO
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

-- type ParamErrors = Array String

newtype QueryParamKey = QueryParamKey String
derive instance newtypeQueryParamKey :: Newtype QueryParamKey _

class ParseQueryParam a where
  parseQueryParam :: QueryParamKey -> Maybe String -> Either String a

nonExistantKeyError :: QueryParamKey -> String
nonExistantKeyError k = "Mandatory query parameter '" <> unwrap k <> "' is not present."

parseMandatory :: forall a. (String -> Either String a) -> QueryParamKey -> Maybe String -> Either String a
parseMandatory parser k (Just s) = parser s
parseMandatory parser k Nothing = Left $ nonExistantKeyError k

instance parseQueryParamString :: ParseQueryParam String where
  parseQueryParam = parseMandatory Right

instance parseQueryParamInt :: ParseQueryParam Int where
  parseQueryParam k (Just s) = case Int.fromString s of
    (Just i) -> Right i
    Nothing -> Left $ unwrap k <> ": Must be an integer."
  parseQueryParam k Nothing = Left $ nonExistantKeyError k

instance parseQueryParamMaybe :: (ParseQueryParam a) => ParseQueryParam (Maybe a) where
  parseQueryParam k Nothing = Right Nothing
  parseQueryParam k v = Just <$> parseQueryParam k v

parseQueryParams :: forall row list. GDecodeQueryParams row list => RL.RowToList row list => Object String -> Either (Array String) (Record row)
parseQueryParams object = gDecodeQueryParams object (RLProxy :: RLProxy list)

parseNewtype :: forall w u. (Newtype w u) => (ParseQueryParam u) => QueryParamKey -> (Maybe String) -> Either String w
parseNewtype k = parseQueryParam k >>> map wrap

class GDecodeQueryParams (row :: # Type) (list :: RL.RowList) | list -> row where
  gDecodeQueryParams :: FO.Object String -> RLProxy list -> Either (Array String) (Record row)

instance gDecodeQueryParamsNil :: GDecodeQueryParams () RL.Nil where
  gDecodeQueryParams _ _ = Right {}

-- TODO accumulate error messages
instance gDecodeQueryParamsCons
  :: ( ParseQueryParam value
     , GDecodeQueryParams rowTail tail
     , IsSymbol field
     , Row.Cons field value rowTail row
     , Row.Lacks field rowTail
     )
  => GDecodeQueryParams row (RL.Cons field value tail) where

  gDecodeQueryParams object _ = do
    let sProxy :: SProxy field
        sProxy = SProxy

        fieldName = reflectSymbol sProxy
        restResult = gDecodeQueryParams object (RLProxy :: RLProxy tail)
        valResult = parseQueryParam (wrap fieldName) (FO.lookup fieldName object)
    case (Tuple valResult restResult) of
      (Tuple (Left valErr) (Left resErrs)) -> Left ([valErr] <> resErrs)
      (Tuple (Left valErr) (Right res)) -> Left [valErr]
      (Tuple (Right val) (Left resErrs)) -> Left resErrs
      (Tuple (Right val) (Right res))  -> pure $ Record.insert sProxy val res
