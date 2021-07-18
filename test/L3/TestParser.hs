{-# LANGUAGE LambdaCase #-}

module L3.TestParser (tests) where
    import TestUtil
    import L3.Parser


    tests :: [IO ()]
    tests = [ testParser
            ]

    parseT :: String -> Result ShowExpr
    parseT = fmapR parseExpr . lexSrc

    assertRight :: Result ShowExpr -> String -> IO ()
    assertRight x = assertTrue pred
            where pred = case x of
                       Right _ -> True
                       Left _ -> False

    testParser :: IO ()
    testParser = do
        assertRight (parseT "*") "Parse star * atom"
        assertRight (parseT "#") "Parse box # atom"

        assertRight (parseT "x") "Parse variable x atom"
        assertRight (parseT "x@y") "Parse namespaced variable x@y atom"

        assertRight (parseT "λ (a : *) -> a") "Parse lambda λ(a:*) -> a expression"
        assertRight (parseT "π (a : *) -> a") "Parse pi π(a:*) -> a expression"
        assertRight (parseT "π (a : *) -> a -> a") "Parse anonymous pi π(a:*) -> a -> a expression"

        assertRight (parseT "f x") "Parse application f x expression"

        assertRight (parseT "lambda (a : m b -> c) -> d") "Monadic anonymous (applicative) pi expression"

        putStrLn $ showExpr $ throwL $ parseT "lambda (f : a -> b -> c) -> f"


