let conf = ./spago.dhall

in      conf
    //  { dependencies =
              conf.dependencies
            # [ "arrays"
              , "benchotron"
              , "bifunctors"
              , "effect"
              , "free"
              , "matryoshka"
              , "quickcheck"
              , "psci-support"
              , "tailrec"
              , "typelevel-prelude"
              ]
        , sources = conf.sources # [ "benchmarks/**/*.purs" ]
        }
