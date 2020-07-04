module Server.OAuth
( OAuth
, RedirectBackTo )
where

import Effect.Aff
import Data.Either (Either)

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

type OAuth codeType userDetailsPayload = {
  redirect :: RedirectBackTo -> String
, handleCode :: codeType -> RedirectBackTo -> Aff (Either String userDetailsPayload)
}
