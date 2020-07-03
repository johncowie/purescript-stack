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
  , "crypto"
  , "datetime"
  , "effect"
  , "exceptions"
  , "formatters"
  , "httpure"
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
