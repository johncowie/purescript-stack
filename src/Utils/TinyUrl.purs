module Utils.TinyUrl where

import Effect.Exception.Unsafe (unsafeThrow)
import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class.Console (log)

req :: AX.Request String
req = AX.defaultRequest { url = "http://tinyurl.com/api-create.php?url=http://www.johncowie.co.uk&callback=?",
                          method = Left GET,
                          responseFormat = ResponseFormat.string,
                          headers = []
                         }

main :: Effect Unit
main = void $ launchAff $ do
  result <- AX.request req
  case result of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> log $ "GET /api response: " <> response.body

getTinyUrl :: String -> Effect String
getTinyUrl s = unsafeThrow "implement me"
