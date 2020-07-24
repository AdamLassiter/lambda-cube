module Test where
    import Core
    import Pretty
    import Util

    test :: IO ()
    test = do
        -- show strings without " "
        let showE = showExpr id
        let showC = showCtx id

        -- id = λ x:* -> λ y:x -> y :: π x:* -> π y:x -> x
        let term1' = Lam "x" Star (Lam "y" (Var "x") (Var "y"))
        -- >> assume (Bool :: *") (False :: Bool)
        let ctx' = [("Bool", Star), ("False", Var ("Bool"))]

        -- 'compile'
        let (term1, ctx) = throwL$ index term1' ctx'
        let eval1 = term1
        let type1 = Pi 0 Star (Pi 1 (Var 0) (Var 0))
        -- assert
        putStrLn $ "term: " ++ showE term1'
        putStrLn $ "ctx:  " ++ showC ctx'
        let norm1 = normalize term1
        putStrLn $ "eval: " ++ showE (named (assertEquals norm1 eval1) ctx ctx')
        -- putStrLn $ "ctx:  " ++ showCtx ctx
        let inf1 = throwL $ typeIn ctx norm1
        putStrLn $ "type: " ++ showE (named (assertEquals inf1 type1) ctx ctx')
        putStrLn ""
        
        -- id Bool = λ y:Bool -> y :: π y:Bool -> Bool
        let term2' = term1' `App` Var "Bool"

        -- 'compile'
        let (term2, ctx) = throwL $ index term2' ctx'
        let eval2 = normalize term2
        let type2 = either (\x -> Box) id (typeIn ctx eval2)
        -- assert
        putStrLn $ "term: " ++ showE term2'
        putStrLn $ "ctx:  " ++ showC ctx'
        let norm2 = normalize term2
        putStrLn $ "eval: " ++ showE (named (assertEquals norm2 eval2) ctx ctx')
        let inf2 = throwL $ typeIn ctx norm2
        putStrLn $ "type: " ++ showE (named (assertEquals inf2 type2) ctx ctx')
        putStrLn ""

        -- id Bool False = False :: Bool
        let term3' = term2' `App` Var "False"

        -- 'compile'
        let (term3, ctx) = throwL $ index term3' ctx'
        let eval3 = normalize term3
        let type3 = either (\x -> Box) id (typeIn ctx eval3)
        -- assert
        putStrLn $ "term: " ++ showE term3'
        putStrLn $ "ctx:  " ++ showC ctx'
        let norm3 = normalize term3
        putStrLn $ "eval: " ++ showE (named (assertEquals norm3 eval3) ctx ctx')
        let inf3 = throwL $ typeIn ctx norm3
        putStrLn $ "type: " ++ showE (named (assertEquals inf3 type3) ctx ctx')
        putStrLn ""

        return ()
