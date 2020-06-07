{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "bigints"
  , "console"
  , "datetime"
  , "effect"
  , "exceptions"
  , "formatters"
  , "httpure"
  , "js-timers"
  , "node-process"
  , "numbers"
  , "parsing"
  , "postgresql-client"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
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
