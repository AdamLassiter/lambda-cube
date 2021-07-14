module Main (main) where
    import qualified L3.TestCore
    import qualified Morte.TestMorte

    main :: IO ()
    main = sequence_ $ Morte.TestMorte.tests ++ L3.TestCore.tests
