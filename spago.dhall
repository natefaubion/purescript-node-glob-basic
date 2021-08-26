{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "refs"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
