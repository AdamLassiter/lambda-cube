module L3.TestCore (tests) where
    import TestUtil
    import L3.Core


    tests :: [IO ()]
    tests = [ testShowCtx
            , testFree
            , testSubstitute
            , testNormalize
            , testIndex
            ]

    testShowCtx :: IO ()
    testShowCtx = do
        assertEq (showCtx ([] :: Context Int)) "" "Show empty context"
        assertEq (showCtx ([(1, Star)] :: Context Int)) "\n(1,Star)" "Show one-elem context"


    testFree :: IO ()
    testFree = do
        assertTrue (free 0 Star) "0 is free in Star"
        assertTrue (free 0 Box) "0 is free in Box"

        assertTrue (free 0 (Var 0)) "0 is free in Var 0"
        assertFalse (free 0 (Var 1)) "0 is not free in Var 1"

        assertTrue (free 0 (Lam 0 (Var 1) (Var 1))) "0 is free in Lam 0 : 1 . 1"
        assertTrue (free 0 (Lam 1 Star Star)) "0 is free in Lam 1 : a . b if free in a and b"
        assertFalse (free 0 (Lam 1 (Var 1) Star)) "0 is not free in Lam 1 : 1 . _"
        assertFalse (free 0 (Lam 1 Star (Var 1))) "0 is not free in Lam 1 : _ . 1"

        assertTrue (free 0 (Pi 0 (Var 1) (Var 1))) "0 is free in Pi 0 : 1 . 1"
        assertTrue (free 0 (Pi 1 Star Star)) "0 is free in Pi 1 : a . b if free in a and b"
        assertFalse (free 0 (Pi 1 (Var 1) Star)) "0 is not free in Pi 1 : 1 . _"
        assertFalse (free 0 (Pi 1 Star (Var 1))) "0 is not free in Pi 1 : _ . 1"

        assertTrue (free 0 (Star `App` Star)) "0 is free in a b if free in a and b"
        assertFalse (free 0 (Var 1 `App` Star)) "0 is not free in 1 _"
        assertFalse (free 0 (Star `App` Var 1)) "0 is not free in _ 1"


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
    testAlphaEq = undefined
