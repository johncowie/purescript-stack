module Bricks where

import Prelude
import Bricker (cacheBustFileNames, compileTasks, cmd, chainTasks, task, contentHash, rewriteLinksInHtml)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..))
import Data.Either (Either(..))
import Data.String (toLower)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Aff (launchAff_)

lowercaseId :: String -> Either String String
lowercaseId s = Right (toLower s)

main :: Effect Unit
main =
  launchAff_
    $ compileTasks lowercaseId do
        cmd "clean" "rm -f frontend-dist/js/* && rm -f frontend-dist/index.html"
        cmd "build-html" "cp html/index.html frontend-dist/index.html"
        cmd "bundle-js" "spago bundle-app --main Main --to frontend-dist/js/frontend.js"
        cmd "browserify" "browserify frontend-dist/js/frontend.js -o frontend-dist/js/frontend.bundle.js"
        cmd "deploy" "cd frontend-dist && git add . && git commit -m \"New build\" && git push -f origin gh-pages"
        chainTasks "build-js" [ "bundle-js", "browserify", "hash-js" ]
        chainTasks "build" [ "clean", "build-html", "build-js", "deploy" ]
        cmd "clean-server" "rm -f server-dist/*.js"
        cmd "build-server" "spago bundle-app --main Server.Main --to server-dist/server.js"
        cmd "bundle-server" "noderify server-dist/server.js -o server-dist/server.bundle.js"
        cmd "deploy-server" """cd server-dist && git add . && git commit --allow-empty -m "New build" && git push -f heroku server-builds:master"""
        chainTasks "release-server" [ "clean-server", "build-server", "bundle-server", "deploy-server" ]
        task "hash-js" $ liftEffect
          $ runExceptT do
              rewrites <- ExceptT $ cacheBustFileNames contentHash (wrap "frontend-dist/js") (wrap "frontend-dist/js")
              ExceptT $ Right <$> Console.log (show rewrites)
              ExceptT $ rewriteLinksInHtml rewrites (wrap "frontend-dist/index.html")
