module Bricker where

import Prelude

import Bricker.File as F

import Control.Monad.State as S
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Map as M
import Data.String (toLower, length)
import Data.String as Str
import Data.String.Regex as Re
import Data.String.Regex.Flags as ReFlags
import Data.Array (drop, catMaybes, head, last, init)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.List (List(..), (:))
import Data.List as List
import Data.Foldable (foldM)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy)


import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (try)

import Node.Process as Node
import Node.FS.Sync as FS
import Node.Encoding (Encoding(UTF8))
import Node.Crypto.Hash (Algorithm(MD5), hex) as Hash

import Utils.Exec (execAff')
import Utils.Wrapper (Wrapper(..), rewrap)

data Task id = Action (ExceptT String Aff Unit)
             | Chain (Array id)
             -- | Parallel (Array id)

type TaskDirectory id = M.Map id (Task id)

type TaskBuilder id = S.State (TaskDirectory id) Unit

task :: forall id res. (Ord id) => id -> Aff (Either String res) -> TaskBuilder id
task id action = S.modify_ (M.insert id (Action (void $ ExceptT action)))

chainTasks :: forall id. (Ord id) => id -> Array id -> TaskBuilder id
chainTasks id tasks = S.modify_ (M.insert id (Chain tasks))

cmd :: forall id. (Ord id) => id -> String -> TaskBuilder id
cmd id bash = S.modify_ (M.insert id (Action (ExceptT action)))
  where action = do
          {stdout, stderr} <- execAff' bash
          if length stdout > 0
            then liftEffect $ Console.log stdout
            else pure unit
          if length stderr > 0
            then liftEffect $ Console.error stderr
            else pure unit
          pure (Right unit)

-- parallel :: forall id. (Ord id) => id -> Array id -> TaskBuilder id
-- parallel id tasks = S.modify_ (M.insert id (Parallel tasks))
-- -- launch with aff

runTask :: forall id. (Ord id) => (Show id) => TaskDirectory id -> Task id -> ExceptT String Aff Unit
runTask taskDir (Chain ids) = void $ for ids (runTaskId taskDir)
runTask taskDir (Action action) = action

runTaskId :: forall id. (Ord id) => (Show id) => TaskDirectory id -> id -> ExceptT String Aff Unit
runTaskId taskDir id =
  case M.lookup id taskDir of
    (Just t) -> do
      liftEffect $ Console.log $ "Running task [" <> show id <> "]"
      runTask taskDir t
      liftEffect $ Console.log $ "Finished running task [" <> show id <> "]"
    Nothing -> ExceptT $ pure $ Left $ "No task registered with ID [" <> show id <> "]"

loggerTask :: String -> Aff (Either String Unit)
loggerTask msg = do
  liftEffect $ Console.log msg
  pure (Right unit)

logError :: Aff (Either String Unit) -> Aff Unit
logError e = do
  rE <- e
  case rE of
    (Left err) -> liftEffect $ Console.error err
    (Right u) -> pure u

compileTasks :: forall id. (Ord id) => (Show id) => (String -> Either String id) -> TaskBuilder id -> Aff Unit
compileTasks idReader taskBuilder = logError $ runExceptT do
  args <- ExceptT $ Right <$> liftEffect Node.argv
  ids <- ExceptT $ pure $ sequence $ map idReader (drop 2 args)
  let taskDir = S.execState taskBuilder M.empty
  void $ for ids (runTaskId taskDir)

--- end of library

type FileHash = Wrapper (SProxy "FileHash") String
type FileName = Wrapper (SProxy "FileName") String
type DirPath = Wrapper (SProxy "DirPath") String
type RelFileLink = Wrapper (SProxy "RelFileLink") String
type CWD = Wrapper (SProxy "CWD") String

readDir :: DirPath -> Effect (Either String (Array FileName))
readDir dirPath = (lmap show) <$> runExceptT do
  files <- ExceptT $ try $ FS.readdir (unwrap dirPath)
  pure (map wrap files)

fromFileName :: DirPath -> FileName -> F.FilePath
fromFileName dir fn = wrap $ unwrap dir <> "/" <> unwrap fn

toFileName :: F.FilePath -> FileName
toFileName fp = wrap $ fromMaybe (unwrap fp) $ last $ Str.split (Str.Pattern "/") $ unwrap fp

toDir :: F.FilePath -> DirPath
toDir fp = wrap $ (Str.joinWith "/" $ dropLast $ Str.split (Str.Pattern "/") $ unwrap fp) <> "/"

dropLast :: forall a. Array a -> Array a
dropLast = fromMaybe [] <<< init

setFileName :: FileName -> F.FilePath -> F.FilePath
setFileName (Wrapper fn) (Wrapper fp) =
  case Str.split (Str.Pattern "/") fp of
    [] -> (wrap fn)
    arr -> wrap $ Str.joinWith "/" (dropLast arr) <> "/" <> fn

readFiles :: DirPath -> Effect (Either String (Array (Tuple FileName F.TextFile)))
readFiles dir = runExceptT $ do
  files <- ExceptT $ readDir dir
  for files \f -> do
    let filePath = fromFileName dir f
    text <- ExceptT $ F.readFile filePath
    pure (Tuple f text)

writeFile :: F.FilePath -> F.TextFile -> Effect (Either String Unit)
writeFile fp content = (lmap show) <$> (try $ FS.writeTextFile UTF8 (unwrap fp) (unwrap content))

writeFiles :: Array (Tuple F.FilePath F.TextFile) -> Effect Unit
writeFiles files = void $ for files \(Tuple fp content) -> do
  writeFile fp content

insertHashSuffix :: FileName -> FileHash -> Either String FileName
insertHashSuffix fileName fileHash = do
  let flags = ReFlags.ignoreCase
  regex <- Re.regex "(.*)\\.([a-z0-9]*)$" flags
  Right case toArray <$> Re.match regex (unwrap fileName) of
    (Just [fn, (Just prefix), (Just suffix)]) -> wrap $ prefix <> "." <> unwrap fileHash <> "." <> suffix
    _ -> fileName

contentHash :: F.TextFile -> Effect FileHash
contentHash textFile = do
  h <- Hash.hex Hash.MD5 (unwrap textFile)
  pure $ wrap h

cacheBustFileNames :: (F.TextFile -> Effect FileHash) -> DirPath -> DirPath -> Effect (Either String (M.Map F.FilePath F.FilePath))
cacheBustFileNames suffixGen inputDir outputDir = runExceptT do
  inputDirAbs <- ExceptT $ F.realPath (rewrap inputDir)
  outputDirAbs <- ExceptT $ F.realPath (rewrap outputDir)
  ExceptT $ Right <$> Console.log (show outputDirAbs)
  files <- ExceptT $ readFiles inputDir
  entries <- for files \(Tuple fileName content) -> do
    suffix <- ExceptT $ Right <$> suffixGen content
    newFileName <- ExceptT $ pure $ insertHashSuffix fileName suffix
    let newFilePath = fromFileName (rewrap outputDirAbs) newFileName
    ExceptT $ writeFile newFilePath content
    pure $ Tuple (fromFileName (rewrap inputDirAbs) fileName) newFilePath
  pure $ M.fromFoldable entries

nonEmptyMatches :: Maybe (NonEmptyArray (Maybe String)) -> Array String
nonEmptyMatches (Just arr) = catMaybes $ toArray arr
nonEmptyMatches Nothing = []

htmlLinks :: F.TextFile -> Either String (Array RelFileLink)
htmlLinks (Wrapper s) = do
  let regexPattern = "(src|href)\\s*=\\s*\"(.*)\""
  linkRegex <- Re.regex regexPattern $ ReFlags.global <> ReFlags.ignoreCase
  oneLinkRegex <- Re.regex regexPattern ReFlags.ignoreCase
  let matches = nonEmptyMatches $ Re.match linkRegex s
  let links = catMaybes $ map (Re.match oneLinkRegex >>> nonEmptyMatches >>> drop 2 >>> head) matches
  pure (map wrap links)

rewriteLink :: M.Map F.FilePath F.FilePath -> F.FilePath -> F.TextFile -> RelFileLink -> Effect (Either String F.TextFile)
rewriteLink rewrites filePath fileContents link = runExceptT do
  let fileDir = toDir filePath
  (absPath :: F.FilePath) <- ExceptT $ F.realPath $ wrap $ (unwrap fileDir <> unwrap link)
  case M.lookup absPath rewrites of
    (Just rewritePath) -> do
      let relPath = linkRelativeToFile filePath rewritePath
      ExceptT $ Right <$> (Console.log $ "Rewriting link: " <> (unwrap link) <> " to " <> unwrap relPath <> " in file " <> unwrap link)
      pure $ wrap $ Str.replaceAll (Str.Pattern (unwrap link)) (Str.Replacement (unwrap relPath)) $ unwrap fileContents
    Nothing -> pure fileContents

rewriteLinksInHtml :: M.Map F.FilePath F.FilePath -> F.RelFilePath -> Effect (Either String Unit)
rewriteLinksInHtml rewrites htmlPath = runExceptT do
  absPath <- ExceptT $ F.realPath htmlPath
  htmlFile <- ExceptT $ F.readFile absPath
  relLinks <- ExceptT $ pure $ htmlLinks htmlFile
  updatedHtml <- foldM (\f l -> ExceptT $ rewriteLink rewrites absPath f l) htmlFile relLinks
  ExceptT $ writeFile absPath updatedHtml

currentDir :: Effect CWD
currentDir = wrap <$> Node.cwd

relativePath :: List String -> List String -> List String
relativePath a b = relativePathRec Nil a b
  where relativePathRec builder Nil Nil = builder
        relativePathRec builder Nil (y:ys) = relativePathRec (builder <> y:Nil) Nil ys
        relativePathRec builder (x:xs) Nil = relativePathRec (upOne:builder) xs Nil
        relativePathRec builder (x:xs) (y:ys) =
          if (x == y && builder == Nil)
            then relativePathRec Nil xs ys
            else relativePathRec (upOne:builder <> y:Nil) xs ys
        upOne = ".."

splitDirPath :: String -> List String
splitDirPath = Str.split (Str.Pattern "/") >>> dropLast >>> List.fromFoldable

joinDirPath :: List String -> String
joinDirPath = List.toUnfoldable >>> Str.joinWith "/" >>> (\s -> s <> "/")

linkRelativeToFile :: F.FilePath -> F.FilePath -> F.RelFilePath
linkRelativeToFile srcFilePath linkedFilePath = wrap (path <> fn)
  where fn = unwrap (toFileName linkedFilePath)
        path = joinDirPath $ relativePath (splitDirPath (unwrap srcFilePath)) (splitDirPath (unwrap linkedFilePath))

lowercaseId :: String -> Either String String
lowercaseId s = Right (toLower s)

main :: Effect Unit
main = launchAff_ $ compileTasks lowercaseId do
  cmd "clean" "rm -f frontend-dist/js/* && rm -f frontend-dist/index.html"
  cmd "build-html" "cp html/index.html frontend-dist/index.html"
  cmd "bundle-js" "spago bundle-app --main Main --to frontend-dist/js/frontend.js"
  cmd "browserify" "node_modules/browserify/bin/cmd.js frontend-dist/js/frontend.js -o frontend-dist/js/frontend.bundle.js"
  cmd "deploy" "cd frontend-dist && git add . && git commit -m \"New build\" && git push -f origin gh-pages"
  chainTasks "build-js" ["bundle-js", "browserify", "hash-js"]
  chainTasks "build" ["clean", "build-html", "build-js", "deploy"]
  cmd "clean-server" "rm -f server-dist/*.js"
  cmd "build-server" "spago bundle-app --main Server.Main --to server-dist/server.js"
  cmd "bundle-server" "node_modules/noderify/bin.js server-dist/server.js -o server-dist/server.bundle.js"
  cmd "deploy-server" """cd server-dist && git add . && git commit --allow-empty -m "New build" && git push -f heroku server-builds:master"""
  chainTasks "release-server" ["clean-server", "build-server", "bundle-server", "deploy-server"]
  task "hash-js" $ liftEffect $ runExceptT do
    rewrites <- ExceptT $ cacheBustFileNames contentHash (wrap "frontend-dist/js") (wrap "frontend-dist/js")
    ExceptT $ Right <$> Console.log (show rewrites)
    ExceptT $ rewriteLinksInHtml rewrites (wrap "frontend-dist/index.html")
