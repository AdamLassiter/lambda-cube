module Main (main) where
    import qualified L3.TestUtil
    import qualified L3.TestCore
    import qualified L3.TestLexer
    import qualified L3.TestLoader
    import qualified L3.TestParser
    import qualified Morte.TestMorte

    import L3.Logging


    main :: IO ()
    main = withStdoutLogging $ do
        setLogLevel LevelInfo
        sequence_ tests

    tests :: [IO ()]
    tests = foldl1 (++) [ L3.TestUtil.tests
                        , L3.TestCore.tests
                        , L3.TestLexer.tests
                        , L3.TestLoader.tests
                        , L3.TestParser.tests
                        , Morte.TestMorte.tests
                        ]
