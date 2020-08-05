module Utils.Env
  ( module TypedEnv
  , getEnv
  , fromEnv
  , Env
  ) where

import Prelude
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Node.Process as NP
import Foreign.Object (Object)
import TypedEnv as TE  -- (type (<:), EnvError, fromEnv)
import TypedEnv (type (<:), EnvError, class ReadEnv, class ParseValue)

newtype Env
  = Env (Object String)

derive instance newtypeEnv :: Newtype Env _

getEnv :: Effect Env
getEnv = Env <$> NP.getEnv

fromEnv :: forall e r proxy. ReadEnv e r => proxy e -> Env -> Either EnvError (Record r)
fromEnv proxy (Env env) = TE.fromEnv proxy env
