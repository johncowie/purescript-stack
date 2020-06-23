module Bricker.File
( FilePath
, TextFile
, RelFilePath
, readFile
, realPath)
where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Either (Either)
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy)

import Effect (Effect)
import Effect.Exception (try)

import Node.FS.Sync as FS
import Node.Encoding (Encoding(UTF8))

import Utils.Wrapper (Wrapper)
import Utils.ExceptT (showError)

type FilePath = Wrapper (SProxy "FilePath") String
type TextFile = Wrapper (SProxy "TextFile") String
type RelFilePath = Wrapper (SProxy "RelFilePath") String

readFile :: FilePath -> Effect (Either String TextFile)
readFile filePath = do
  result <- try $ FS.readTextFile UTF8 (unwrap filePath)
  pure $ wrap <$> showError result

realPath :: RelFilePath -> Effect (Either String FilePath)
realPath rp = showError <$> runExceptT do
  res <- ExceptT $ try $ FS.realpath (unwrap rp)
  pure (wrap res)
