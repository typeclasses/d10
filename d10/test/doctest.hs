import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "src/Data/D10/Char.hs"
    ]
