module Server.OAuth
( OAuth
, OAuthCode
, UserData
, RedirectBackTo )
where

import Effect.Aff
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Server.QueryParams (class ParseQueryParam, parseNewtype)

{- Steps
   1. Client -> Server  Request redirect URI to OAuth Provider
   2. Server -> Client  URI to redirect to
   3. Client            redirect to OAuth
   4. OAuth             login user and then redirect back to Client with code
   5. Client -> Server  provide code
   6. Server -> Client  user details and token


   Server OAuth abstraction:
    - endpoint for retrieving redirect
    - endpoint for handling code and fetching details from OAuth provider
-}

type RedirectBackTo = String

newtype OAuthCode = OAuthCode String
derive instance newtypeOAuthCode :: Newtype OAuthCode _

instance oauthCodeParseQueryParam :: ParseQueryParam OAuthCode where
  parseQueryParam = parseNewtype

type UserData = {
  sub :: String
, email :: String
, name :: String
}

type OAuth = {
  redirect :: RedirectBackTo -> String
, handleCode :: OAuthCode -> RedirectBackTo -> Aff (Either String UserData)
}
