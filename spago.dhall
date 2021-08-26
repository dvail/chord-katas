{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "chord-katas"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "random"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "strings"
  , "test-unit"
  , "uuid"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
