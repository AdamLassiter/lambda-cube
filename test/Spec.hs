module Main (main) where
    import qualified L3.TestCore
    import qualified L3.TestLexer
    import qualified L3.TestParser
    import qualified Morte.TestMorte

    main :: IO ()
    main = sequence_ tests

    tests :: [IO ()]
    tests = foldl1 (++) [ L3.TestCore.tests
                        , L3.TestLexer.tests
                        , L3.TestParser.tests
                        , Morte.TestMorte.tests
                        ]
