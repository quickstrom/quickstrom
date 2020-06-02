{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "webcheck"
, dependencies = [ "numbers", "strings", "arrays", "typelevel-prelude", "quickcheck" ]
, packages = ./packages.dhall
, sources = [ "lib/**/*.purs", "test/**/*.purs" ]
}
