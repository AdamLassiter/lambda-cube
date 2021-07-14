module Main (main) where
    import qualified L3.TestCore
    import qualified Morte.TestMorte

    main :: IO ()
    main = sequence_ tests

    tests :: [IO ()]
    tests = foldl1 (++) [ L3.TestCore.tests
                        , Morte.TestMorte.tests
                        ]
