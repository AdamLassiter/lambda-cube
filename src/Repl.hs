module Repl where
    import Core
    import ParseIO

    import System.Console.Haskeline

    main :: IO ()
    main = runInputT defaultSettings repl

    repl :: InputT IO ()
    repl = repl0 []
        where showE = showExpr id
              showC = showCtx id
              repl0 ctx' = do
                input <- getInputLine ">> "
                case input of
                    Just a -> do
                        let res = parseExpr a
                        case res of
                            Right expr' -> do
                                outputStrLn $ "term: " ++ showE expr'
                                outputStrLn $ "ctx:  " ++ showC ctx'
                                let (expr, ctx) = index expr' ctx'
                                let eval = normalize expr
                                outputStrLn $ "eval: " ++ showE (named eval ctx ctx')
                                case (typeIn ctx eval) of
                                    Left err  -> error err
                                    Right inf -> outputStrLn $ "type: " ++ showE (named inf ctx ctx')
                                outputStrLn ""
                            Left err -> error err
                    Nothing -> error "quit"
                repl0 ctx'
