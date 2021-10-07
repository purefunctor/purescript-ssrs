{ name = "ssrs"
, dependencies =
  [ "dissect"
  , "either"
  , "fixed-points"
  , "free"
  , "lists"
  , "prelude"
  , "safe-coerce"
  , "tailrec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/PureFunctor/purescript-ssrs.git"
}
