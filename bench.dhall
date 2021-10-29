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
              ]
        , sources = conf.sources # [ "benchmarks/**/*.purs" ]
        }
