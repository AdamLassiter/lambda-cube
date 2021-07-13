module Main where
    import L3.Core (Expr (Star))
    import L3.Loader (wrapPrelude)

    import Data.Time.Clock

    main = do
        start <- getCurrentTime
        let (tCtx, prel) = wrapPrelude
        let _ = prel Star
        end <- getCurrentTime
        putStrLn $ "WrapPrelude took " ++ show (diffUTCTime end start)
