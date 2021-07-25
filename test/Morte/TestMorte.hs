module Morte.TestMorte (tests) where
    import TestUtil
    import L3.Loader
    import L3.Logging


    tests :: [IO ()]
    tests = [ example0
            , example1
            , example2
            , example3
            , example4
            , example5
            , example6
            , example7
            , example8
            , example9
            -- , example10
            -- , example11
            -- , example12
            -- , example13
            -- , example14
            -- , example15
            ]

    rstrip :: String -> String
    rstrip = reverse . dropWhile (=='\n') . reverse

    parse :: ShowCtx -> String -> (ShowExpr, ShowExpr)
    parse tCtx inp = case fmapR (evalExpr tCtx) prEx of
            Left err -> errorU $ show err
            Right (t, e) -> (t, normalize0 e)
        where prEx = fmapR parseExpr $ lexSrc inp

    example :: FilePath -> String -> String -> IO ()
    example file typ expr = do
        contents <- readFile file
        let (typ', expr') = parse [] expr
        let (typ0, expr0) = parse [] contents
        debugM $ "\n== " ++ show file ++ " =="
        debugM typ
        debugM expr
        debugM "\n== Expression =="
        debugM $ rstrip contents
        debugM "== Type =="
        debugM $ "Expected: " ++ showExpr typ'
        debugM $ "Actual: " ++ showExpr typ0
        assertShowing showExpr (alphaEq, "=α=") typ' typ0 "Expected type is alpha-equivalent to inferred actual type"
        debugM "== Normalization =="
        debugM $ "Expected: " ++ showExpr expr'
        debugM $ "Actual: " ++ showExpr expr0
        assertShowing showExpr (alphaEq, "=α=") expr' expr0 "Expected expression is alpha-equivalent to normalized actual expression"
        return ()


    example0 :: IO ()
    example0 =
        example
            "test/Morte/example0.l3"
            "forall (a : *) . forall (x : a) . a"
            "lambda (a : *) . lambda (x : a) . x"

    example1 :: IO ()
    example1 =
        example
            "test/Morte/example1.l3"
            "forall (String : *) . forall (x : String) . String"
            "lambda (String : *) . lambda (x : String) . x"

    example2 :: IO ()
    example2 =
        example
            "test/Morte/example2.l3"
            "forall (a : *) . a . a"
            "lambda (a : *) . lambda (x : a) . x"

    example3 :: IO ()
    example3 =
        example
            "test/Morte/example3.l3"
            "forall (Int : *) . forall (Zero : Int) . forall (One : Int) . Int"
            "lambda (Int : *) . lambda (Zero : Int) . lambda (One : Int) . One"

    example4 :: IO ()
    example4 =
        example
            "test/Morte/example4.l3"
            "forall (a : *) . forall (x : a) . forall (y : a) . a"
            "lambda (a : *) . lambda (x : a) . lambda (y : a) . y"

    example5 :: IO ()
    example5 =
        example
            "test/Morte/example5.l3"
            "forall (r : *) . r . r . r"
            "lambda (r : *) . lambda (x : r) . lambda (_ : r) . x"

    example6 :: IO ()
    example6 =
        example
            "test/Morte/example6.l3"
            "forall (a : *) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (a . x . x) . x . x"
            "lambda (a : *) . lambda (l : forall (x : *) . (a . x . x) . x . x) . l"

    example7 :: IO ()
    example7 =
        example
            "test/Morte/example7.l3"
            "forall (a : *) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (a . x . x) . x . x"
            "lambda (a : *) . lambda (va : forall (x : *) . (a . x . x) . x . x) . va"

    example8 :: IO ()
    example8 =
        example
            "test/Morte/example8.l3"
            "forall (a : *) . forall (b : *) . forall (c : *) . forall (f : b . c) . forall (g : a . b) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (c . x . x) . x . x"
            "lambda (a : *) . lambda (b : *) . lambda (c : *) . lambda (f : b . c) . lambda (g : a . b) . lambda (l : forall (x : *) . (a . x . x) . x . x) . lambda (x : *) . lambda (Cons : c . x . x) . l x (lambda (va : a) . Cons (f (g va)))"

    example9 :: IO ()
    example9 =
        example
            "test/Morte/example9.l3"
            "forall (a : *) . forall (b : *) . forall (c : *) . forall (f : b . c) . forall (g : a . b) . (forall (x : *) . (a . x . x) . x . x) . forall (x : *) . (c . x . x) . x . x"
            "lambda (a : *) . lambda (b : *) . lambda (c : *) . lambda (f : b . c) . lambda (g : a . b) . lambda (va : forall (x : *) . (a . x . x) . x . x) . lambda (x : *) . lambda (Cons : c . x . x) . va x (lambda (va : a) . Cons (f (g va)))"

    example10 :: IO ()
    example10 =
        example
            "test/Morte/example10.l3"
            "forall (a : *) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x"
            "lambda (a : *) . lambda (st : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . st"

    example11 :: IO ()
    example11 =
        example
            "test/Morte/example11.l3"
            "forall (a : *) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x"
            "lambda (a : *) . lambda (va : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . va"

    example12 :: IO ()
    example12 =
        example
            "test/Morte/example12.l3"
            "forall (a : *) . forall (b : *) . forall (c : *) . (b . c) . (a . b) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . x"
            "lambda (a : *) . lambda (b : *) . lambda (c : *) . lambda (f : b . c) . lambda (g : a . b) . lambda (st : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . lambda (x : *) . lambda (S : forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . st x (lambda (s : *) . lambda (seed : s) . lambda (step : s . forall (x : *) . (a . s . x) . x) . S s seed (lambda (seed : s) . lambda (x : *) . lambda (Pair : c . s . x) . step seed x (lambda (va : a) . Pair (f (g va)))))"

    example13 :: IO ()
    example13 =
        example
            "test/Morte/example13.l3"
            "forall (a : *) . forall (b : *) . forall (c : *) . (b . c) . (a . b) . (forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . x"
            "lambda (a : *) . lambda (b : *) . lambda (c : *) . lambda (f : b . c) . lambda (g : a . b) . lambda (va : forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (a . s . x) . x) . x) . x) . lambda (x : *) . lambda (S : forall (s : *) . s . (s . forall (x : *) . (c . s . x) . x) . x) . va x (lambda (s : *) . lambda (seed : s) . lambda (step : s . forall (x : *) . (a . s . x) . x) . S s seed (lambda (seed : s) . lambda (x : *) . lambda (Pair : c . s . x) . step seed x (lambda (va : a) . Pair (f (g va)))))"

    example14 :: IO ()
    example14 =
        example
            "test/Morte/example14.l3"
            "forall (String : *) . forall (U : *) . forall (Unit : U) . forall (x : *) . (String . x . x) . ((String . x) . x) . (U . x) . x"
            "lambda (String : *) . lambda (U : *) . lambda (Unit : U) . lambda (x : *) . lambda (PutStrLn : String . x . x) . lambda (GetLine : (String . x) . x) . lambda (Return : U . x) . GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (GetLine (lambda (va : String) . PutStrLn va (Return Unit))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))"

    example15 :: IO ()
    example15 =
        example
            "test/Morte/example15.l3"
            "forall (String : *) . forall (r : *) . forall (x : *) . (forall (s : *) . s . (s . forall (x : *) . (String . s . x) . ((String . s) . x) . (r . x) . x) . x) . x"
            "lambda (String : *) . lambda (r : *) . lambda (x : *) . lambda (k : forall (s : *) . s . (s . forall (x : *) . (String . s . x) . ((String . s) . x) . (r . x) . x) . x) . k (forall (x : *) . (String . x) . x . x) (lambda (x : *) . lambda (Just : String . x) . lambda (Nothing : x) . Nothing) (lambda (m : forall (x : *) . (String . x) . x . x) . m (forall (x : *) . (String . (forall (x : *) . (String . x) . x . x) . x) . ((String . forall (x : *) . (String . x) . x . x) . x) . (r . x) . x) (lambda (str : String) . lambda (x : *) . lambda (PutStrLn : String . (forall (x : *) . (String . x) . x . x) . x) . lambda (GetLine : (String . forall (x : *) . (String . x) . x . x) . x) . lambda (Return : r . x) . PutStrLn str (lambda (x : *) . lambda (Just : String . x) . lambda (Nothing : x) . Nothing)) (lambda (x : *) . lambda (PutStrLn : String . (forall (x : *) . (String . x) . x . x) . x) . lambda (GetLine : (String . forall (x : *) . (String . x) . x . x) . x) . lambda (Return : r . x) . GetLine (lambda (va : String) . lambda (x : *) . lambda (Just : String . x) . lambda (Nothing : x) . Just va)))"
