{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "quickstrom"
, dependencies = [ "numbers", "strings", "arrays", "typelevel-prelude", "lcg", "transformers", "generics-rep", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
