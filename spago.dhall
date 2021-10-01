{ name = "ssrs"
, dependencies =
  [ "console"
  , "dissect"
  , "effect"
  , "either"
  , "fixed-points"
  , "lists"
  , "prelude"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/PureFunctor/purescript-ssrs.git"
}
