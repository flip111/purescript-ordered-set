{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ordered-set"
, dependencies =
    [ "arrays", "console", "effect", "nonempty", "psci-support", "unfoldable" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "AGPL-3.0"
, repository = "https://github.com/flip111/purescript-ordered-set"
}
