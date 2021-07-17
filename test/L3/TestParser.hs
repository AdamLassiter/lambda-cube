{-# LANGUAGE LambdaCase #-}

module L3.TestParser (tests) where
    import TestUtil
    import L3.Parser


    tests :: [IO ()]
    tests = [ testParser
            ]

    testParser :: IO ()
    testParser = do
        let parseT = fmapR parseExpr . lexSrc

        let x = parseT "π (a : *) -> a -> a"
        print x
        assertTrue ((\case
            Right _ -> True
            Left _ -> False) x) ""
