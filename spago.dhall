{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "graphql-parser"
, repository = "https://github.com/meeshkan/purescript-graphql-parser"
, license = "Apache-2.0"
, dependencies =
  [ "console", "effect", "generics-rep", "numbers", "parsing", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
