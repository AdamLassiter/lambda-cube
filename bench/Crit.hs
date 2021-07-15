module Main where
    import L3.Core
    import L3.Loader

    import Data.Time.Clock

    main = do
        let (tCtx, prel) = wrapPrelude
        let prelExpr = prel $ Lam (Name "x") Star (Var $ Name "x")
        start <- getCurrentTime
        let idTy = throwL $ inferType0 prelExpr
        mid <- getCurrentTime
        let idEx = normalize0 prelExpr
        end <- getCurrentTime
        putStrLn $ "Infer took " ++ show (diffUTCTime mid start) ++ " to deduce " ++ show idTy
        putStrLn $ "Normalize took " ++ show (diffUTCTime end mid) ++ " to deduce " ++ show idEx
