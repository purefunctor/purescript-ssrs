let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220418/packages.dhall
        sha256:2523a5659d0f3b198ffa2f800da147e0120578842e492a7148e4b44f357848b3

let overrides =
      { dissect =
              upstream.dissect
          //  { dependencies =
                    upstream.dissect.dependencies
                  # [ "foreign-object", "variant" ]
              , version = "main"
              }
      }

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
        , repo = "https://github.com/hdgarrood/purescript-benchotron.git"
        , version = "e64664de1fa0843ca78949f36b31b176fa6b0f84"
        }
      }

in  upstream // overrides // additions
