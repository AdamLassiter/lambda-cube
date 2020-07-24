module Test where
    import Repl
    import Util

    import System.Console.Haskeline

    stlc :: IO ()
    stlc = do
        runInputTBehavior (useFile "./test/stlc.lc") defaultSettings repl

    test :: IO ()
    test = do
        stlc
        return ()
