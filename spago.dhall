{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "elm-licenses"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "either"
  , "maybe"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "psci-support"
  , "simple-json"
  , "generics-rep"
  , "foreign"
  , "foldable-traversable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
