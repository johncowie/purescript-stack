module Bricker.File
( FilePath
, TextFile
, RelFilePath
, readFile
, realPath)
where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Newtype (unwrap, wrap)

import Effect (Effect)
import Effect.Exception (try)

import Node.FS.Sync as FS
import Node.Encoding (Encoding(UTF8))

import Utils.Wrapper (Wrapper)

type FilePath = Wrapper "FilePath" String
type TextFile = Wrapper "TextFile" String
type RelFilePath = Wrapper "RelFilePath" String

readFile :: FilePath -> Effect (Either String TextFile)
readFile filePath = do
  result <- try $ FS.readTextFile UTF8 (unwrap filePath)
  pure $ wrap <$> (lmap show) result

realPath :: RelFilePath -> Effect (Either String FilePath)
realPath rp = (lmap show) <$> runExceptT do
  res <- ExceptT $ try $ FS.realpath (unwrap rp)
  pure (wrap res)
