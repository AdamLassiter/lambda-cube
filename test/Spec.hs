module Main (main) where
    import L3.Pretty (showExpr, ShowCtx, ShowExpr, evalExpr1, normalize0, fmapR, mapR)
    import L3.Lexer (lexSrc)
    import L3.Parser (parseExpr)
    import L3.Loader (wrapPrelude)

    main :: IO ()
    main = do sequence_ tests

    parse :: ShowCtx -> (ShowExpr -> ShowExpr) -> String -> (ShowExpr, ShowExpr)
    parse tCtx prel inp = case fmapR (evalExpr1 tCtx) prEx of
            Left err -> error err
            Right (t, e) -> (t, normalize0 e)
        where prEx = mapR prel $ fmapR parseExpr $ lexSrc inp

    example :: FilePath -> String -> String -> IO ()
    example file typ expr = do
        contents <- readFile file
        (tCtx, prel) <- wrapPrelude
        let (typ', expr') = parse tCtx prel expr
        let (typ0, expr0) = parse tCtx prel contents
        putStrLn "=========="
        putStrLn file
        putStrLn typ
        putStrLn expr
        putStrLn "=========="
        putStrLn "Expression:"
        putStrLn contents
        putStrLn "== Type =="
        putStrLn $ "Expected: " ++ showExpr typ'
        putStrLn $ "Actual: " ++ showExpr typ0
        if typ' == typ0
          then putStrLn "PASS"
          else error "FAIL"
        putStrLn "== Expr =="
        putStrLn $ "Expected: " ++ showExpr expr'
        putStrLn $ "Actual: " ++ showExpr expr0
        if expr' == expr0
          then putStrLn "PASS"
          else error "FAIL"
        putStrLn ""
        return ()


    tests :: [IO ()]
    tests = [ example0,
              example1,
              example2,
              example3,
              example4,
              example5,
              example6,
              example7,
              example8,
              example9,
              example10,
              example11,
              example12,
              example13,
              example14,
              example15 ]


    example0 :: IO ()
    example0 =
        example
            "test/morte/example0.mt"
            "forall (a : *) . forall (x : a) . a"
            "lambda (a : *) . lambda (x : a) . x"

    example1 :: IO ()
    example1 =
        example
            "test/morte/example1.mt"
            "forall (Text : *) . forall (x : Text) . Text"
            "lambda (Text : *) . lambda (x : Text) . x"

    example2 :: IO ()
    example2 =
        example
            "test/morte/example2.mt"
            "forall (a : *) . a . a"
            "lambda (a : *) . lambda (x : a) . x"

    example3 :: IO ()
    example3 =
        example
            "test/morte/example3.mt"
            "forall (Int : *) . forall (Zero : Int) . forall (One : Int) . Int"
            "lambda (Int : *) . lambda (Zero : Int) . lambda (One : Int) . One"

    example4 :: IO ()
    example4 =
        example
            "test/morte/example4.mt"
            "forall (a : *) . forall (x : a) . forall (y : a) . a"
            "lambda (a : *) . lambda (x : a) . lambda (y : a) . y"

    example5 :: IO ()
    example5 =
        example
            "test/morte/example5.mt"
            "forall (r : *) . r . r . r"
            "lambda (r : *) . lambda (x : r) . lambda (_ : r) . x"

    example6 :: IO ()
    example6 =
        example
            "test/morte/example6.mt"
            "forall (a : *) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (a . x . x) . x . x"
            "lambda (a : *) . lambda (l : forall (x : *) . (a . x . x) . x . x) . l"

    example7 :: IO ()
    example7 =
        example
            "test/morte/example7.mt"
            "forall (a : *) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (a . x . x) . x . x"
            "lambda (a : *) . lambda (va : forall (x : *) . (a . x . x) . x . x) . va"

    example8 :: IO ()
    example8 =
        example
            "test/morte/example8.mt"
            "forall (a : *) . forall (b : *) . forall (c : *) . forall (f : b . c) . forall (g : a . b) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (c . x . x) . x . x"
            "lambda (a : *) . lambda (b : *) . lambda (c : *) . lambda (f : b . c) . lambda (g : a . b) . lambda (l : forall (x : *) . (a . x . x) . x . x) . lambda (x : *) . lambda (Cons : c . x . x) . l x (lambda (va : a) . Cons (f (g va)))"

    example9 :: IO ()
    example9 =
        example
            "test/morte/example9.mt"
            "forall (a : *) . forall (b : *) . forall (c : *) . forall (f : b . c) . forall (g : a . b) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (c . x . x) . x . x"
            "lambda (a : *) . lambda (b : *) . lambda (c : *) . lambda (f : b . c) . lambda (g : a . b) . lambda (va : forall (x : *) . (a . x . x) . x . x) . lambda (x : *) . lambda (Cons : c . x . x) . va x (lambda (va : a) . Cons (f (g va)))"

    example10 :: IO ()
    example10 =
        example
            "test/morte/example10.mt"
            "forall (a : *) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x"
            "lambda (a : *) . lambda (st : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . st"

    example11 :: IO ()
    example11 =
        example
            "test/morte/example11.mt"
            "forall (a : *) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x"
            "lambda (a : *) . lambda (va : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . va"

    example12 :: IO ()
    example12 =
        example
            "test/morte/example12.mt"
            "forall (a : *) . forall (b : *) . forall (c : *) . (b . c) . (a . b) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . x"
            "lambda (a : *) . lambda (b : *) . lambda (c : *) . lambda (f : b . c) . lambda (g : a . b) . lambda (st : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . lambda (x : *) . lambda (S : forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . st x (lambda (s : *) . lambda (seed : s) . lambda (step : s . forall (x : *) . (a . s . x) . x) . S s seed (lambda (seed : s) . lambda (x : *) . lambda (Pair : c . s . x) . step seed x (lambda (va : a) . Pair (f (g va)))))"

    example13 :: IO ()
    example13 =
        example
            "test/morte/example13.mt"
            "forall (a : *) . forall (b : *) . forall (c : *) . (b . c) . (a . b) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . x"
            "lambda (a : *) . lambda (b : *) . lambda (c : *) . lambda (f : b . c) . lambda (g : a . b) . lambda (va : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . lambda (x : *) . lambda (S : forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . va x (lambda (s : *) . lambda (seed : s) . lambda (step : s . forall (x : *) . (a . s . x) . x) . S s seed (lambda (seed : s) . lambda (x : *) . lambda (Pair : c . s . x) . step seed x (lambda (va : a) . Pair (f (g va)))))"

    example14 :: IO ()
    example14 =
        example
            "test/morte/example14.mt"
            "forall (Text : *) . forall (U : *) . forall (Unit : U) . forall (x : *) . (Text . x . x) . ((Text . x) . x) . (U . x) . x"
            "lambda (Text : *) . lambda (U : *) . lambda (Unit : U) . lambda (x : *) . lambda (PutStrLn : Text . x . x) . lambda (GetLine : (Text . x) . x) . lambda (Return : U . x) . GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (GetLine (lambda (va : Text) . PutStrLn va (Return Unit))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))"

    example15 :: IO ()
    example15 =
        example
            "test/morte/example15.mt"
            "forall (Text : *) . forall (r : *) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (Text . s . x) . ((Text . s) . x) . (r . x) . x) . x) . x"
            "lambda (Text : *) . lambda (r : *) . lambda (x : *) . lambda (k : forall (s : *) . s . (s . forall (x : *) . (Text . s . x) . ((Text . s) . x) . (r . x) . x) . x) . k (forall (x : *) . (Text . x) . x . x) (lambda (x : *) . lambda (Just : Text . x) . lambda (Nothing : x) . Nothing) (lambda (m : forall (x : *) . (Text . x) . x . x) . m (forall (x : *) . (Text . (forall (x : *) . (Text . x) . x . x) . x) . ((Text . forall (x : *) . (Text . x) . x . x) . x) . (r . x) . x) (lambda (str : Text) . lambda (x : *) . lambda (PutStrLn : Text . (forall (x : *) . (Text . x) . x . x) . x) . lambda (GetLine : (Text . forall (x : *) . (Text . x) . x . x) . x) . lambda (Return : r . x) . PutStrLn str (lambda (x : *) . lambda (Just : Text . x) . lambda (Nothing : x) . Nothing)) (lambda (x : *) . lambda (PutStrLn : Text . (forall (x : *) . (Text . x) . x . x) . x) . lambda (GetLine : (Text . forall (x : *) . (Text . x) . x . x) . x) . lambda (Return : r . x) . GetLine (lambda (va : Text) . lambda (x : *) . lambda (Just : Text . x) . lambda (Nothing : x) . Just va)))"
