module Main where
    import L3.Loader

    import Criterion.Main

    wrapPreludeIO :: ShowExpr -> IO ShowExpr
    wrapPreludeIO expr = do
        (tCtx, prel) <- wrapPrelude <$> embeddedPreludeIO
        let id' = prel expr
        pure id'

    main = defaultMain [ bench "λ(x:*).x" $ whnfIO $ wrapPreludeIO $ Lam (Name "x") Star (Var $ Name "x")
                       , bench "x" $ whnfIO $ wrapPreludeIO $ Var $ Name "x"
                       , bench "λ(x:Nat).Bool@eq (even x) (odd (Nat@Succ x))" $ whnf (parseExpr . throwL . lexSrc) "lambda (x : forall (Nat : *) -> forall (Succ : forall (_ : Nat) -> Nat) -> forall (Zero : Nat) -> Nat) -> (((((((((x) (forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool)) (lambda (x : forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool) -> (((x) (forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool)) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> False)) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> True))) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> False)) (forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool)) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> False)) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> True)) (forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool)) ((((x) (forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool)) (lambda (x : forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool) -> (((x) (forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool)) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> False)) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> True))) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> True))) (((((((x) (forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool)) (lambda (x : forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool) -> (((x) (forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool)) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> False)) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> True))) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> True)) (forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool)) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> False)) (lambda (Bool : *) -> lambda (True : Bool) -> lambda (False : Bool) -> True))"
                       ]
