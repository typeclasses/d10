import System.Environment
import System.Process

main =
  do
    ghc <- getEnv "ghc"

    let constraints = case ghc of
            "8.2.2"  -> [ "base" .= "4.10.*"
                        , "hedgehog" .= "1.0"
                        , "template-haskell" .= "2.12.*"
                        ]
            "8.4.3"  -> [ "base" .= "4.11.*"
                        , "hedgehog" .= "1.0.1"
                        , "template-haskell" .= "2.13.*"
                        ]
            "8.6.1"  -> [ "base" .= "4.12.*"
                        , "hedgehog" .= "1.0.2"
                        , "template-haskell" .= "2.14.*"
                        ]
            "8.8.1"  -> [ "base" .= "4.13.*"
                        , "hedgehog" .= "1.0.3"
                        , "template-haskell" .= "2.15.*"
                        ]
            "8.10.1" -> [ "base" .= "4.14.*"
                        , "hedgehog" .= "1.0.5"
                        , "template-haskell" .= "2.16.*"
                        ]

    callProcess "cabal" ("build" : "all" : constraints)
    callProcess "cabal" ("test" : "all" : "--enable-tests" : constraints)

x .= y =
    "--constraint=" ++ x ++ "==" ++ y
