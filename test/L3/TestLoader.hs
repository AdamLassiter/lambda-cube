module L3.TestLoader (tests) where
    import TestUtil
    import L3.Loader

    import System.FilePath


    tests :: [IO ()]
    tests = [ testTakeDirectoryName
            , testTakeNamespacedFileName
            , testEmbeddedPrelude
            , testLoadPrelude
            , testWrapPrelude
            ]

    showSome :: (Show a, Show b) => [(a, b)] -> String
    showSome [] = "[  ]"
    showSome x  = "[" ++ show (fst $ head x) ++ " .. [+" ++ show (length x - 2) ++ " elements] .. " ++ show (fst $ last x) ++ "]"

    assertListEq :: (Eq a, Show a, Eq b, Show b) => [(a, b)] -> [(a, b)] -> String -> IO ()
    assertListEq = assertShowing showSome ((==), "==")

    assertListNotEmpty :: (Eq a, Show a, Eq b, Show b) => [(a, b)] -> String -> IO ()
    assertListNotEmpty x = assertShowing showSome ((/=), "/=") x []

    assertBetaEq :: ShowExpr -> ShowExpr -> String -> IO ()
    assertBetaEq = assertShowing (showExpr . normalize0) (betaEq, "=β=")

    testTakeDirectoryName :: IO ()
    testTakeDirectoryName = do
        let fp = "root" </> "directory" </> "file" <.> "ext"
        assertEq (takeDirectoryName fp) "directory" "Take directory from filepath"

    testTakeNamespacedFileName :: IO ()
    testTakeNamespacedFileName = do
        let noNs = "." </> "file" <.> "ext"
        assertEq (takeNamespacedFileName noNs) "file" "Take filename when not namespaced"
        let ns = "Bool" </> "file" <.> "ext"
        assertEq (takeNamespacedFileName ns) "Bool@file" "Take dir@filename when namespaced"

    testEmbeddedPrelude :: IO ()
    testEmbeddedPrelude = do
        ioPrelude <- embeddedPreludeIO
        assertListNotEmpty ioPrelude "IO Prelude is non-empty"
        assertListNotEmpty embeddedPrelude "Prelude is non-empty"
        assertListEq embeddedPrelude ioPrelude "Prelude is IO Prelude"

    testLoadPrelude :: IO ()
    testLoadPrelude = do
        let (τCtx, eCtx) = loadPrelude embeddedPrelude
        assertListNotEmpty τCtx "Type-context is non-empty"
        assertListNotEmpty eCtx "Expression-context is non-empty"

    testWrapPrelude :: IO ()
    testWrapPrelude = do
        let (τCtx, prelWrap) = wrapPrelude embeddedPrelude
        assertBetaEq (prelWrap $ throwL $ parseExpr $ throwL $ lexSrc "id") (throwL $ parseExpr $ throwL $ lexSrc "λ(a:*) -> λ(x:a) -> x") "Prelude@id =α= id"
