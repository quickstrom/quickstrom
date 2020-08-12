{ name = "webcheck-web-frontend"
, dependencies = [ "numbers", "strings", "arrays", "console", "halogen" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
