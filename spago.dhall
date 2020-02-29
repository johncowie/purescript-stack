{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut"
  , "console"
  , "datetime"
  , "effect"
  , "formatters"
  , "js-timers"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "spork"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
