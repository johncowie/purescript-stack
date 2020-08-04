module Server.Middleware.FormURLEncoded
( formDataValues
, formDataValue
, class DecodeFormData
, decodeFormData
, wrapFormURLEncoded
, wrapDecodeFormURLEncoded )
where

import Prelude

import Data.Array (filter, catMaybes)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..), note)
import Data.FormURLEncoded (FormURLEncoded, decode, toArray)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

import Global (decodeURIComponent)

import JohnCowie.HTTPure (class IsRequest)
import JohnCowie.HTTPure as Req
import JohnCowie.Data.Lens as L

fixPluses :: String -> String
fixPluses = Str.replaceAll (Str.Pattern "+") (Str.Replacement "%20")

decodeURLEncoded :: String -> String
decodeURLEncoded s = fromMaybe s $ decodeURIComponent s

formDataValues :: String -> FormURLEncoded -> Either String (NonEmptyArray String)
formDataValues k = toArray
                   >>> filter (fst >>> (==) k)
                   >>> map snd
                   >>> catMaybes
                   >>> map decodeURLEncoded
                   >>> NonEmpty.fromArray
                   >>> note ("No values for key '" <> k <> "'")

formDataValue :: String -> FormURLEncoded -> Either String String
formDataValue k formData = do
  vals <- formDataValues k formData
  case NonEmpty.toArray vals of
    [single] -> Right single
    _ -> Left $ "Multiple values for key '" <> k <> "'"

class DecodeFormData a where
  decodeFormData :: FormURLEncoded -> Either String a

wrapFormURLEncoded :: forall a req res.
                      (IsRequest req)
                   => (Functor req)
                   => (String -> res)
                   -> (req (FormURLEncoded /\ a) -> res)
                   -> req a
                   -> res
wrapFormURLEncoded parseFail router req = case decode (fixPluses body) of
  (Just formData) -> router $ map (Tuple formData) req
  Nothing -> parseFail "Couldn't parse FormURLEncoded data"
  where body = L.view Req._body req

wrapDecodeFormURLEncoded :: forall a b req res.
                            (IsRequest req)
                         => (Functor req)
                         => (DecodeFormData b)
                         => (String -> res)
                         -> (req (b /\ a) -> res)
                         -> req (FormURLEncoded /\ a)
                         -> res
wrapDecodeFormURLEncoded parseFail router req = case decodeFormData formData of
  (Left err) -> parseFail err
  (Right val) -> router $ map (\(formData /\ a) -> val /\ a) req
  where (formData /\ _) = L.view Req._val req
