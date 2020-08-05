{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "b64"
  , "bigints"
  , "browser-cookies"
  , "console"
  , "coroutines"
  , "crypto"
  , "datetime"
  , "debug"
  , "effect"
  , "exceptions"
  , "form-urlencoded"
  , "formatters"
  , "httpure"
  , "johncowie-bricker"
  , "johncowie-httpure"
  , "johncowie-jwt"
  , "johncowie-oauth"
  , "johncowie-postgres"
  , "johncowie-simple-lens"
  , "js-timers"
  , "node-child-process"
  , "node-fs"
  , "node-process"
  , "numbers"
  , "parsing"
  , "postgresql-client"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "spec"
  , "spork"
  , "these"
  , "typedenv"
  , "uri"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
