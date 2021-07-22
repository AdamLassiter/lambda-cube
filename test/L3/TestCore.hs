module L3.TestCore (tests) where
    import TestUtil
    import L3.Core


    tests :: [IO ()]
    tests = [ testShowCtx
            , testFree
            , testSubstitute
            , testNormalize
            , testIndex
            , testAlphaEq
            , testBetaEq
            , testInferType
            ]

    assertAlphaEq :: (Show a, Eq a) => Expr a -> Expr a -> String -> IO ()
    assertAlphaEq = assert (alphaEq, "=α=")

    assertNotAlphaEq :: (Show a, Eq a) => Expr a -> Expr a -> String -> IO ()
    assertNotAlphaEq = assert ((not .) . alphaEq, "=α=")

    assertBetaEq :: (Show a, Eq a) => Expr a -> Expr a -> String -> IO ()
    assertBetaEq = assert (betaEq, "=β=")

    assertNotBetaEq :: (Show a, Eq a) => Expr a -> Expr a -> String -> IO ()
    assertNotBetaEq = assert ((not .) . betaEq, "=β=")


    testShowCtx :: IO ()
    testShowCtx = do
        assertEq (showCtx ([] :: Context Int)) "" "Show empty context"
        assertEq (showCtx ([(1, Star)] :: Context Int)) "\n(1,Star)" "Show one-elem context"


    testFree :: IO ()
    testFree = do
        assertFalse (free 0 Star) "0 is not free in Star"
        assertFalse (free 0 Box) "0 is not free in Box"

        assertTrue (free 0 (Var 0)) "0 is free in Var 0"
        assertFalse (free 0 (Var 1)) "0 is not free in Var 1"

        assertFalse (free 0 (Lam 0 (Var 1) (Var 1))) "0 is not free in Lam 0 : 1 . 1"
        assertTrue (free 0 (Lam 0 (Var 0) (Var 1))) "0 is free in Lam 0 : 0 . 1"
        assertFalse (free 0 (Lam 0 (Var 1) (Var 0))) "0 is not free in Lam 0 : 1 . 0"
        assertTrue (free 0 (Lam 1 (Var 0) Star)) "0 is free in Lam 1 : 0 . *"
        assertTrue (free 0 (Lam 1 Star (Var 0))) "0 is free in Lam 1 : 0 . *"
        assertFalse (free 0 (Lam 1 (Var 1) (Var 1))) "0 is not free in Lam 1 : 1 . 1"

        assertFalse (free 0 (Pi 0 (Var 1) (Var 1))) "0 is not free in Pi 0 : 1 . 1"
        assertTrue (free 0 (Pi 0 (Var 0) (Var 1))) "0 is free in Pi 0 : 0 . 1"
        assertFalse (free 0 (Pi 0 (Var 1) (Var 0))) "0 is not free in Pi 0 : 1 . 0"
        assertTrue (free 0 (Pi 1 (Var 0) Star)) "0 is free in Pi 1 : 0 . *"
        assertTrue (free 0 (Pi 1 Star (Var 0))) "0 is free in Pi 1 : 0 . *"
        assertFalse (free 0 (Pi 1 (Var 1) (Var 1))) "0 is not free in Pi 1 : 1 . 1"

        assertFalse (free 0 (Star `App` Star)) "0 is not free in * *"
        assertTrue (free 0 (Var 0 `App` Star)) "0 is free in 0 *"
        assertTrue (free 0 (Star `App` Var 0)) "0 is free in * 0"
        assertTrue (free 0 (Var 0 `App` Var 0)) "0 is free in 0 0"


    testSubstitute :: IO ()
    testSubstitute = do
        assertEq (substitute 0 (Var 1) Star) Star "*[0 := 1] leaves * unchanged"
        assertEq (substitute 0 (Var 1) Box)  Box "#[0 := 1] leaves # unchanged"

        assertEq (substitute 0 (Var 1) (Var 0)) (Var 1) "0[0 := 1] renames to 1"
        assertEq (substitute 0 (Var 1) (Var 2)) (Var 2) "2[0 := 1] leaves 2 unchanged"

        assertEq (substitute 0 (Var 1) (Lam 0 (Var 0) (Var 0))) (Lam 0 (Var 1) (Var 0)) "(Lam 0 : 0 . 0)[0 := 1] renames free type context to Lam 0 : 1 . 0"
        assertEq (substitute 0 (Var 1) (Lam 2 (Var 0) (Var 0))) (Lam 2 (Var 1) (Var 1)) "(Lam 2 : 0 . 0)[0 := 1] renames all to Lam 2 : 1 . 1"

        assertEq (substitute 0 (Var 1) (Pi 0 (Var 0) (Var 0))) (Pi 0 (Var 1) (Var 0)) "(Pi 0 : 0 . 0)[0 := 1] renames free type context to Pi 0 : 1 . 0"
        assertEq (substitute 0 (Var 1) (Pi 2 (Var 0) (Var 0))) (Pi 2 (Var 1) (Var 1)) "(Pi 2 : 0 . 0)[0 := 1] renames all to Pi 2 : 1 . 1"

        assertEq (substitute 0 (Var 1) (Var 0 `App` Var 0)) (Var 1 `App` Var 1) "(0 0)[0 := 1] renames to 1 1"


    testNormalize :: IO ()
    testNormalize = do
        -- parametric expression e over variable x of type a
        let param a x e = Lam a Star (Lam x (Var a) e)
        assertEq (normalize (param 0 1 $ param 2 3 (Var 3) `App` Var 0 `App` Var 1)) (param 0 1 (Var 1)) "Id Id normalizes to Id"


    testIndex :: IO ()
    testIndex = do
        assertEq (index0 (Var "x")) (Var $ Right "x") "Unbound names cannot be indexed"

        assertEq (index0 $ Lam "x" Star (Var "x")) (Lam (Left 0) Star (Var $ Left 0)) "Bound names can be indexed"
        assertEq (index0 $ Lam "x" (Var "x") (Var "x")) (Lam (Left 0) (Var $ Right "x") (Var $ Left 0)) "Types are unbound relative to their binding"

        assertEq (index0 $ Pi "x" Star (Var "x")) (Pi (Left 0) Star (Var $ Left 0)) "Bound names can be indexed"
        assertEq (index0 $ Pi "x" (Var "x") (Var "x")) (Pi (Left 0) (Var $ Right "x") (Var $ Left 0)) "Types are unbound relative to their binding"

        assertEq (index0 $ Var "x" `App` Var "y") (Var (Right "x") `App` Var (Right "y")) "Unbound names cannot be indexed over applications"
        assertEq (index0 $ Lam "x" Star (Var "x") `App` Lam "y" Star (Var "y")) (Lam (Left 0) Star (Var $ Left 0) `App` Lam (Left 0) Star (Var $ Left 0)) "Bound names can be indexed over applications"


    testAlphaEq :: IO ()
    testAlphaEq = do
        assertAlphaEq (Var "x") (Var "x") "Matching unbound names are alpha-equivalent"
        assertNotAlphaEq (Var "x") (Var "y") "Non-matching unbound names are not alpha-equivalent"

        assertAlphaEq (Lam "x" Star (Var "x")) (Lam "y" Star (Var "y")) "Bound names are alpha-equivalent over lambdas"
        assertAlphaEq (Pi "x" Star (Var "x")) (Pi "y" Star (Var "y")) "Bound names are alpha-equivalent over pis"

        assertAlphaEq (Lam "x" Star (Var "x") `App` Var "y") (Lam "y" Star (Var "y") `App` Var "y") "Alpha-equivalence distributes over application"


    testBetaEq :: IO ()
    testBetaEq = do
        assertBetaEq (Lam "A" Star (Lam "X" (Var "A") (Lam "a" Star (Lam "x" (Var "a") (Var "x")) `App` Var "A" `App` Var "X"))) (Lam "a" Star (Lam "x" (Var "a") (Var "x"))) "Id Id is equivalent to Id"


    testInferType :: IO ()
    testInferType = do
        assertEq (inferType0 (Star :: Expr String)) (Right Box) "* :: #"

        assertEq (inferType [("x", Star)] (Var "x")) (Right Star) "x :: * ⊢ x :: *"

        assertEq (inferType0 (Lam "x" Star (Var "x"))) (Right $ Pi "x" Star Star) "lam x : * -> x :: pi x : * -> *"
        assertEq (inferType0 (Lam "x" Star (Var "x"))) (Right $ Pi "x" Star Star) "lam x : * -> x :: pi x : * -> *"

        assertEq (inferType [("a", Star)] (Pi "x" (Var "a") (Var "a"))) (Right Star) "a :: * ⊢ pi x : a -> a :: *"
        assertEq (inferType0 (Pi "x" Star (Var "x"))) (Right Star) "pi x : * -> x :: *"
        assertEq (inferType [("a", Star)] (Pi "x" (Var "a") Star)) (Right Box) "a :: * ⊢ pi x : * -> * :: #"
        assertEq (inferType0 (Pi "x" Star Star)) (Right Box) "pi x : * -> * :: #"
