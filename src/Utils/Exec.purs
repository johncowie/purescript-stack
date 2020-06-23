module Utils.Exec where

import Node.ChildProcess as CP

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, Canceler)
import Effect.Exception (Error)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))

type ProcessOutput = { stdout :: String, stderr :: String }

{-
_affCallback
  :: forall e
   . (Error -> Eff (buffer :: BUFFER | e) Unit)
  -> (ProcessOutput -> Eff (buffer :: BUFFER | e) Unit)
  -> (CP.ExecResult -> Eff (buffer :: BUFFER | e) Unit)
_affCallback reject accept =
  \({stdout, stderr, error}) ->
        case error of
          Just e -> reject e
          Nothing -> do
            out <- Buffer.toString UTF8 stdout
            err <- Buffer.toString UTF8 stderr
            accept {stdout: out, stderr: err}
-}

_affCallback :: String -> CP.ExecOptions -> ((Either Error ProcessOutput) -> Effect Unit) -> Effect Canceler
_affCallback bash options = \callback -> do
  childProcess <- CP.exec bash options \({stdout, stderr, error}) -> do
    case error of
      (Just e) -> callback (Left e)
      Nothing -> do
        out <- Buffer.toString UTF8 stdout
        err <- Buffer.toString UTF8 stderr
        callback (Right {stdout: out, stderr: err})
  mempty

execAff :: String -> CP.ExecOptions -> Aff ProcessOutput
execAff bash options = makeAff (_affCallback bash options)

execAff' :: String -> Aff ProcessOutput
execAff' bash = execAff bash CP.defaultExecOptions
