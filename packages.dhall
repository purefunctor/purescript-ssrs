let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220504/packages.dhall
        sha256:fd37736ecaa24491c907af6a6422156417f21fbf25763de19f65bd641e8340d3

let overrides =
      { dissect =
              upstream.dissect
          //  { dependencies =
                    upstream.dissect.dependencies
                  # [ "foreign-object", "variant" ]
              , version = "v1.0.0"
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
