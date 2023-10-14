{ name = "node-glob-basic"
, license = "MIT"
, repository = "https://github.com/natefaubion/purescript-node-glob-basic.git"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "node-fs"
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
