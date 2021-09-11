{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "chord-katas"
, dependencies =
  [ "aff"
  , "arrays"
  , "effect"
  , "elmish"
  , "elmish-html"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "strings"
  , "test-unit"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
