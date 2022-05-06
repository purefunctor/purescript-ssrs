let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220505/packages.dhall
        sha256:ba57c25c86fd54c2b672cda3a6836bbbdff4b1bbf946bceaabb64e5a10285638

let overrides = {=}

let additions =
      { benchotron =
        { dependencies =
          [ "arrays"
          , "console"
          , "datetime"
          , "effect"
          , "exceptions"
          , "exists"
          , "foldable-traversable"
          , "identity"
          , "lcg"
          , "node-fs"
          , "node-readline"
          , "now"
          , "numbers"
          , "profunctor"
          , "quickcheck"
          , "strings"
          , "transformers"
          ]
        , repo = "https://github.com/PureFunctor/purescript-benchotron.git"
        , version = "v0.15.0"
        }
      }

in  upstream // overrides // additions
