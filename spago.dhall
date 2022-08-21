{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "graphql-parser"
, repository = "https://github.com/meeshkan/purescript-graphql-parser"
, license = "Apache-2.0"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "numbers"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
