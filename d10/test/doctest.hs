import           Test.DocTest

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "src/Data/D10/Char.hs"
    , "src/Data/D10/Num.hs"
    , "src/Data/D10/Predicate.hs"
    , "src/Data/D10/Safe.hs"
    ]
