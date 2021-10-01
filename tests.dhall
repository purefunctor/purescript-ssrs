let conf = ./spago.dhall

in      conf
    //  { dependencies = conf.dependencies # [ "psci-support" ]
        , sources = conf.sources # [ "test/**/*.purs" ]
        }
