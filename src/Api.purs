module Api where

import Prelude
import Data.Either (Either)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Utils.HttpClient (getJson)
import Utils.AppendStore (ApiRoot)

getOAuthRedirect :: ApiRoot -> String -> Aff (Either Error { redirect :: String })
getOAuthRedirect apiRoot redirect = getJson (unwrap apiRoot <> "/login?redirect=" <> redirect)

getAccessToken :: ApiRoot -> String -> String -> Aff (Either Error { accessToken :: String })
getAccessToken apiRoot code redirect = getJson (unwrap apiRoot <> "/google?redirect=" <> redirect <> "&code=" <> code)

-- type EventsApi r = {
--
-- }
