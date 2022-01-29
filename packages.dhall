let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211028/packages.dhall
        sha256:df6486e7fad6dbe724c4e2ee5eac65454843dce1f6e10dc35e0b1a8aa9720b26

let overrides =
      { dissect =
        { dependencies =
          [ "bifunctors"
          , "console"
          , "effect"
          , "fixed-points"
          , "foreign-object"
          , "functors"
          , "lists"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "safe-coerce"
          , "st"
          , "tailrec"
          , "type-equality"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "variant"
          ]
        , repo = "https://github.com/PureFunctor/purescript-dissect.git"
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
