{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where
    import Prelude (IO, sequence, return, ($))
    import System.IO (FilePath)
    import GHC.IO.Encoding
    import System.Exit (exitFailure, exitSuccess)
    import Data.Text
    import Data.Text.IO (putStrLn, readFile)

    main :: IO ()
    main = do
        setLocaleEncoding utf8
        results <- sequence tests
        return ()

    example :: FilePath -> Text -> Text -> IO ()
    example file typ expr = do
        contents <- readFile file
        putStrLn "=========="
        putStrLn $ pack file
        putStrLn "=========="
        putStrLn $ concat ["Expression: ", contents]
        putStrLn "== Type =="
        putStrLn $ concat ["Expected: ", typ]
        putStrLn $ concat ["Actual: ", "none"]
        putStrLn "== Expr =="
        putStrLn $ concat ["Expected: ", expr]
        putStrLn $ concat ["Actual: ", "none"]
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
    --          example9,
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
            "∀(a : *) . ∀(x : a) . a"
            "λ(a : *) . λ(x : a) . x"

    example1 :: IO ()
    example1 =
        example
            "test/morte/example1.mt"
            "∀(Text : *) . ∀(x : Text) . Text"
            "λ(Text : *) . λ(x : Text) . x"

    example2 :: IO ()
    example2 =
        example
            "test/morte/example2.mt"
            "∀(a : *) . a . a"
            "λ(a : *) . λ(x : a) . x"

    example3 :: IO ()
    example3 =
        example
            "test/morte/example3.mt"
            "∀(Int : *) . ∀(Zero : Int) . ∀(One : Int) . Int"
            "λ(Int : *) . λ(Zero : Int) . λ(One : Int) . One"

    example4 :: IO ()
    example4 =
        example
            "test/morte/example4.mt"
            "∀(a : *) . ∀(x : a) . ∀(y : a) . a"
            "λ(a : *) . λ(x : a) . λ(y : a) . y"

    example5 :: IO ()
    example5 =
        example
            "test/morte/example5.mt"
            "∀(r : *) . r . r . r"
            "λ(r : *) . λ(x : r) . λ(_ : r) . x"

    example6 :: IO ()
    example6 =
        example
            "test/morte/example6.mt"
            "∀(a : *) . (∀(x : *) . (a . x . x) . x . x) . ∀(x : *) . (a . x . x) . x . x"
            "λ(a : *) . λ(l : ∀(x : *) . (a . x . x) . x . x) . l"

    example7 :: IO ()
    example7 =
        example
            "test/morte/example7.mt"
            "∀(a : *) . (∀(x : *) . (a . x . x) . x . x) . ∀(x : *) . (a . x . x) . x . x"
            "λ(a : *) . λ(va : ∀(x : *) . (a . x . x) . x . x) . va"

    example8 :: IO ()
    example8 =
        example
            "test/morte/example8.mt"
            "∀(a : *) . ∀(b : *) . ∀(c : *) . ∀(f : b . c) . ∀(g : a . b) . (∀(x : *) . (a . x . x) . x . x) . ∀(x : *) . (c . x . x) . x . x"
            "λ(a : *) . λ(b : *) . λ(c : *) . λ(f : b . c) . λ(g : a . b) . λ(l : ∀(x : *) . (a . x . x) . x . x) . λ(x : *) . λ(Cons : c . x . x) . l x (λ(va : a) . Cons (f (g va)))"

    example9 :: IO ()
    example9 =
        example
            "test/morte/example9.mt"
            "∀(a : *) . ∀(b : *) . ∀(c : *) . ∀(f : b . c) . ∀(g : a . b) . (∀(x : *) . (a . x . x) . x . x) . ∀(x : *) . (c . x . x) . x . x"
            "λ(a : *) . λ(b : *) . λ(c : *) . λ(f : b . c) . λ(g : a . b) . λ(va : ∀(x : *) . (a . x . x) . x . x) . λ(x : *) . λ(Cons : c . x . x) . va x (λ(va : a) . Cons (f (g va)))"

    example10 :: IO ()
    example10 =
        example
            "test/morte/example10.mt"
            "∀(a : *) . (∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (a . s . x) . x) . x) . x) . ∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (a . s . x) . x) . x) . x"
            "λ(a : *) . λ(st : ∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (a . s . x) . x) . x) . x) . st"

    example11 :: IO ()
    example11 =
        example
            "test/morte/example11.mt"
            "∀(a : *) . (∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (a . s . x) . x) . x) . x) . ∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (a . s . x) . x) . x) . x"
            "λ(a : *) . λ(va : ∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (a . s . x) . x) . x) . x) . va"

    example12 :: IO ()
    example12 =
        example
            "test/morte/example12.mt"
            "∀(a : *) . ∀(b : *) . ∀(c : *) . (b . c) . (a . b) . (∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (a . s . x) . x) . x) . x) . ∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (c . s . x) . x) . x) . x"
            "λ(a : *) . λ(b : *) . λ(c : *) . λ(f : b . c) . λ(g : a . b) . λ(st : ∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (a . s . x) . x) . x) . x) . λ(x : *) . λ(S : ∀(s : *) . s . (s . ∀(x : *) . (c . s . x) . x) . x) . st x (λ(s : *) . λ(seed : s) . λ(step : s . ∀(x : *) . (a . s . x) . x) . S s seed (λ(seed : s) . λ(x : *) . λ(Pair : c . s . x) . step seed x (λ(va : a) . Pair (f (g va)))))"

    example13 :: IO ()
    example13 =
        example
            "test/morte/example13.mt"
            "∀(a : *) . ∀(b : *) . ∀(c : *) . (b . c) . (a . b) . (∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (a . s . x) . x) . x) . x) . ∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (c . s . x) . x) . x) . x"
            "λ(a : *) . λ(b : *) . λ(c : *) . λ(f : b . c) . λ(g : a . b) . λ(va : ∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (a . s . x) . x) . x) . x) . λ(x : *) . λ(S : ∀(s : *) . s . (s . ∀(x : *) . (c . s . x) . x) . x) . va x (λ(s : *) . λ(seed : s) . λ(step : s . ∀(x : *) . (a . s . x) . x) . S s seed (λ(seed : s) . λ(x : *) . λ(Pair : c . s . x) . step seed x (λ(va : a) . Pair (f (g va)))))"

    example14 :: IO ()
    example14 =
        example
            "test/morte/example14.mt"
            "∀(Text : *) . ∀(U : *) . ∀(Unit : U) . ∀(x : *) . (Text . x . x) . ((Text . x) . x) . (U . x) . x"
            "λ(Text : *) . λ(U : *) . λ(Unit : U) . λ(x : *) . λ(PutStrLn : Text . x . x) . λ(GetLine : (Text . x) . x) . λ(Return : U . x) . GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (GetLine (λ(va : Text) . PutStrLn va (Return Unit))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))"

    example15 :: IO ()
    example15 =
        example
            "test/morte/example15.mt"
            "∀(Text : *) . ∀(r : *) . ∀(x : *) . (∀(s : *) . s . (s . ∀(x : *) . (Text . s . x) . ((Text . s) . x) . (r . x) . x) . x) . x"
            "λ(Text : *) . λ(r : *) . λ(x : *) . λ(k : ∀(s : *) . s . (s . ∀(x : *) . (Text . s . x) . ((Text . s) . x) . (r . x) . x) . x) . k (∀(x : *) . (Text . x) . x . x) (λ(x : *) . λ(Just : Text . x) . λ(Nothing : x) . Nothing) (λ(m : ∀(x : *) . (Text . x) . x . x) . m (∀(x : *) . (Text . (∀(x : *) . (Text . x) . x . x) . x) . ((Text . ∀(x : *) . (Text . x) . x . x) . x) . (r . x) . x) (λ(str : Text) . λ(x : *) . λ(PutStrLn : Text . (∀(x : *) . (Text . x) . x . x) . x) . λ(GetLine : (Text . ∀(x : *) . (Text . x) . x . x) . x) . λ(Return : r . x) . PutStrLn str (λ(x : *) . λ(Just : Text . x) . λ(Nothing : x) . Nothing)) (λ(x : *) . λ(PutStrLn : Text . (∀(x : *) . (Text . x) . x . x) . x) . λ(GetLine : (Text . ∀(x : *) . (Text . x) . x . x) . x) . λ(Return : r . x) . GetLine (λ(va : Text) . λ(x : *) . λ(Just : Text . x) . λ(Nothing : x) . Just va)))"
