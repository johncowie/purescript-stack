module Utils.Env
( module TypedEnv
, getEnv
, fromEnv
, Env
)
where

import Prelude
import Data.Either (Either)
import Effect (Effect)
import Node.Process as NP
import Foreign.Object (Object)

import TypedEnv as TE -- (type (<:), EnvError, fromEnv)
import TypedEnv (type (<:), EnvError, class ReadEnv, class ParseValue)

data Env = Env (Object String)

getEnv :: Effect Env
getEnv = Env <$> NP.getEnv

fromEnv :: forall e r proxy. ReadEnv e r => proxy e -> Env -> Either (Array EnvError) (Record r)
fromEnv proxy (Env env) = TE.fromEnv proxy env
