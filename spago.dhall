{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "webcheck"
, dependencies = [ "numbers", "typelevel-prelude" ]
, packages = ./packages.dhall
, sources = [ "lib/**/*.purs", "test/**/*.purs" ]
}
