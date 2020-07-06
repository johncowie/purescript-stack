module Server.OAuth.Stub where

import Prelude
import Data.Either (Either(..))
import Server.OAuth (OAuth)

data StubCode = StubCode String

-- redirect :: RedirectBackTo -> String
-- , handleCode :: OAuthCode -> RedirectBackTo -> Aff (Either String UserData)

oauth :: OAuth
oauth = {
  redirect: \redirect -> redirect <> "?code=1234"
, handleCode: \code redirect -> pure (Right { sub: "100"
                                            , name: "StubUser"
                                            , email: "stub@email.com"})
}
