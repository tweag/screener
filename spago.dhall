{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "screener"
, dependencies =
  [ "aff"
  , "argonaut"
  , "console"
  , "effect"
  , "mmorph"
  , "node-fs-aff"
  , "open-mkdirp-aff"
  , "optparse"
  , "pathy"
  , "psci-support"
  , "toppokki"
  , "variant"
  , "yaml-next"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
