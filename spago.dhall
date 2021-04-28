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
  , "node-fs-aff"
  , "open-mkdirp-aff"
  , "optparse"
  , "pathy"
  , "psci-support"
  , "toppokki"
  , "yaml-next"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
