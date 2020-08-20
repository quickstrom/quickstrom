{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "quickstrom"
, dependencies = [ "numbers", "strings", "arrays", "typelevel-prelude", "lcg", "transformers", "generics-rep" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
