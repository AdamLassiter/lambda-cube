module Main (main) where
    import System.IO (FilePath)
    import System.Exit (exitFailure, exitSuccess)

    main :: IO ()
    main = do
        results <- sequence tests
        return ()

    example :: FilePath -> String -> String -> IO ()
    example file typ expr = do
        contents <- readFile file
        putStrLn "=========="
        putStrLn file
        putStrLn "=========="
        putStrLn $ "Expression: " ++ contents
        putStrLn "== Type =="
        putStrLn $ "Expected: " ++ typ
        putStrLn $ "Actual: " ++ "none"
        putStrLn "== Expr =="
        putStrLn $ "Expected: " ++ expr
        putStrLn $ "Actual: " ++ "none"
        putStrLn ""
        return ()


    tests :: [IO ()]
    tests = [ example0 ]
    --          example1,
    --          example2,
    --          example3,
    --          example4,
    --          example5,
    --          example6,
    --          example7,
    --          example8,
    --          example9
    --          example10,
    --          example11,
    --          example12,
    --          example13,
    --          example14,
    --          example15 ]


    example0 :: IO ()
    example0 =
        example
            "test/morte/example0.mt"
            "forall (a : *) . forall (x : a) . a"
            "\\ (a : *) . \\ (x : a) . x"

    example1 :: IO ()
    example1 =
        example
            "test/morte/example1.mt"
            "forall (Text : *) . forall (x : Text) . Text"
            "\\ (Text : *) . \\ (x : Text) . x"

    example2 :: IO ()
    example2 =
        example
            "test/morte/example2.mt"
            "forall (a : *) . a . a"
            "\\ (a : *) . \\ (x : a) . x"

    example3 :: IO ()
    example3 =
        example
            "test/morte/example3.mt"
            "forall (Int : *) . forall (Zero : Int) . forall (One : Int) . Int"
            "\\ (Int : *) . \\ (Zero : Int) . \\ (One : Int) . One"

    example4 :: IO ()
    example4 =
        example
            "test/morte/example4.mt"
            "forall (a : *) . forall (x : a) . forall (y : a) . a"
            "\\ (a : *) . \\ (x : a) . \\ (y : a) . y"

    example5 :: IO ()
    example5 =
        example
            "test/morte/example5.mt"
            "forall (r : *) . r . r . r"
            "\\ (r : *) . \\ (x : r) . \\ (_ : r) . x"

    example6 :: IO ()
    example6 =
        example
            "test/morte/example6.mt"
            "forall (a : *) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (a . x . x) . x . x"
            "\\ (a : *) . \\ (l : forall (x : *) . (a . x . x) . x . x) . l"

    example7 :: IO ()
    example7 =
        example
            "test/morte/example7.mt"
            "forall (a : *) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (a . x . x) . x . x"
            "\\ (a : *) . \\ (va : forall (x : *) . (a . x . x) . x . x) . va"

    example8 :: IO ()
    example8 =
        example
            "test/morte/example8.mt"
            "forall (a : *) . forall (b : *) . forall (c : *) . forall (f : b . c) . forall (g : a . b) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (c . x . x) . x . x"
            "\\ (a : *) . \\ (b : *) . \\ (c : *) . \\ (f : b . c) . \\ (g : a . b) . \\ (l : forall (x : *) . (a . x . x) . x . x) . \\ (x : *) . \\ (Cons : c . x . x) . l x (\\ (va : a) . Cons (f (g va)))"

    example9 :: IO ()
    example9 =
        example
            "test/morte/example9.mt"
            "forall (a : *) . forall (b : *) . forall (c : *) . forall (f : b . c) . forall (g : a . b) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (c . x . x) . x . x"
            "\\ (a : *) . \\ (b : *) . \\ (c : *) . \\ (f : b . c) . \\ (g : a . b) . \\ (va : forall (x : *) . (a . x . x) . x . x) . \\ (x : *) . \\ (Cons : c . x . x) . va x (\\ (va : a) . Cons (f (g va)))"

    example10 :: IO ()
    example10 =
        example
            "test/morte/example10.mt"
            "forall (a : *) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x"
            "\\ (a : *) . \\ (st : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . st"

    example11 :: IO ()
    example11 =
        example
            "test/morte/example11.mt"
            "forall (a : *) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x"
            "\\ (a : *) . \\ (va : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . va"

    example12 :: IO ()
    example12 =
        example
            "test/morte/example12.mt"
            "forall (a : *) . forall (b : *) . forall (c : *) . (b . c) . (a . b) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . x"
            "\\ (a : *) . \\ (b : *) . \\ (c : *) . \\ (f : b . c) . \\ (g : a . b) . \\ (st : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . \\ (x : *) . \\ (S : forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . st x (\\ (s : *) . \\ (seed : s) . \\ (step : s . forall (x : *) . (a . s . x) . x) . S s seed (\\ (seed : s) . \\ (x : *) . \\ (Pair : c . s . x) . step seed x (\\ (va : a) . Pair (f (g va)))))"

    example13 :: IO ()
    example13 =
        example
            "test/morte/example13.mt"
            "forall (a : *) . forall (b : *) . forall (c : *) . (b . c) . (a . b) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . x"
            "\\ (a : *) . \\ (b : *) . \\ (c : *) . \\ (f : b . c) . \\ (g : a . b) . \\ (va : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . \\ (x : *) . \\ (S : forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . va x (\\ (s : *) . \\ (seed : s) . \\ (step : s . forall (x : *) . (a . s . x) . x) . S s seed (\\ (seed : s) . \\ (x : *) . \\ (Pair : c . s . x) . step seed x (\\ (va : a) . Pair (f (g va)))))"

    example14 :: IO ()
    example14 =
        example
            "test/morte/example14.mt"
            "forall (Text : *) . forall (U : *) . forall (Unit : U) . forall (x : *) . (Text . x . x) . ((Text . x) . x) . (U . x) . x"
            "\\ (Text : *) . \\ (U : *) . \\ (Unit : U) . \\ (x : *) . \\ (PutStrLn : Text . x . x) . \\ (GetLine : (Text . x) . x) . \\ (Return : U . x) . GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (GetLine (\\ (va : Text) . PutStrLn va (Return Unit))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))"

    example15 :: IO ()
    example15 =
        example
            "test/morte/example15.mt"
            "forall (Text : *) . forall (r : *) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (Text . s . x) . ((Text . s) . x) . (r . x) . x) . x) . x"
            "\\ (Text : *) . \\ (r : *) . \\ (x : *) . \\ (k : forall (s : *) . s . (s . forall (x : *) . (Text . s . x) . ((Text . s) . x) . (r . x) . x) . x) . k (forall (x : *) . (Text . x) . x . x) (\\ (x : *) . \\ (Just : Text . x) . \\ (Nothing : x) . Nothing) (\\ (m : forall (x : *) . (Text . x) . x . x) . m (forall (x : *) . (Text . (forall (x : *) . (Text . x) . x . x) . x) . ((Text . forall (x : *) . (Text . x) . x . x) . x) . (r . x) . x) (\\ (str : Text) . \\ (x : *) . \\ (PutStrLn : Text . (forall (x : *) . (Text . x) . x . x) . x) . \\ (GetLine : (Text . forall (x : *) . (Text . x) . x . x) . x) . \\ (Return : r . x) . PutStrLn str (\\ (x : *) . \\ (Just : Text . x) . \\ (Nothing : x) . Nothing)) (\\ (x : *) . \\ (PutStrLn : Text . (forall (x : *) . (Text . x) . x . x) . x) . \\ (GetLine : (Text . forall (x : *) . (Text . x) . x . x) . x) . \\ (Return : r . x) . GetLine (\\ (va : Text) . \\ (x : *) . \\ (Just : Text . x) . \\ (Nothing : x) . Just va)))"
