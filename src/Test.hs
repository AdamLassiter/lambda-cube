module Test where
    import Core
    import Pretty
    import Util

    test :: IO ()
    test = do
        putStrLn $ "Constructions Test Suite"
        putStrLn $ "------------------------"

        -- show strings without " "
        let showE = showExpr id
        let showC = showCtx id

        -- id = λ x:* -> λ y:x -> y :: π x:* -> π y:x -> x
        let term1' = Lam "x" Star (Lam "y" (Var "x") (Var "y"))
        -- >> assume (Bool :: *") (False :: Bool)
        let ctx' = [("Bool", Star), ("False", Var ("Bool"))]

        -- 'compile'
        let (term1, ctx) = index term1' ctx'
        let eval1 = term1
        let type1 = Pi 0 Star (Pi 1 (Var 0) (Var 0))
        -- assert
        putStrLn $ "term: " ++ showE term1'
        putStrLn $ "ctx:  " ++ showC ctx'
        putStrLn $ "eval: " ++ showE (named (assertEquals (normalize term1) eval1) ctx ctx')
        -- putStrLn $ "ctx:  " ++ showCtx ctx
        case (typeOf $ normalize term1) of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ showE (named (assertEquals inf type1) ctx ctx')
        putStrLn ""
        
        -- id Bool = λ y:Bool -> y :: π y:Bool -> Bool
        let term2' = term1' `App` Var "Bool"

        -- 'compile'
        let (term2, ctx) = index term2' ctx'
        let eval2 = normalize term2
        let type2 = either (\x -> Box) id (typeIn ctx eval2)
        -- assert
        putStrLn $ "term: " ++ showE term2'
        putStrLn $ "ctx:  " ++ showC ctx'
        putStrLn $ "eval: " ++ showE (named (assertEquals (normalize term2) eval2) ctx ctx')
        case (typeIn ctx $ normalize term2) of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ showE (named (assertEquals inf type2) ctx ctx')
        putStrLn ""

        -- id Bool False = False :: Bool
        let term3' = term2' `App` Var "False"

        -- 'compile'
        let (term3, ctx) = index term3' ctx'
        let eval3 = normalize term3
        let type3 = either (\x -> Box) id (typeIn ctx eval3)
        -- assert
        putStrLn $ "term: " ++ showE term3'
        putStrLn $ "ctx:  " ++ showC ctx'
        putStrLn $ "eval: " ++ showE (named (assertEquals (normalize term3) eval3) ctx ctx')
        case (typeIn ctx $ normalize term3) of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ showE (named (assertEquals inf type3) ctx ctx')
        putStrLn ""

        return ()
