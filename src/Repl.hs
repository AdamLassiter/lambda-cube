module Repl where
    import Core
    import Pretty
    import Parsec
    import Util

    import Control.Applicative hiding (some, many)
    import System.Console.Haskeline

    data ReplAction = None
                    | Quit String
                    | EvalOf NamedExpr
                    | TypeOf NamedExpr
                    | EchoOf NamedExpr
                    | LetCtx NamedExpr


    -- parsing equivalent to pretty shows

    parseExpr :: String -> Result NamedExpr
    parseExpr = runParser expr
        where expr = star
                <|> box
                <|> var
                <|> lam
                <|> pi
                <|> app
              star = do
                  reserved "*"
                  return Star
              box  = do
                  reserved "#"
                  return Box
              var  = do
                  v <- word
                  return $ Var v
              lam  = do
                  reserved "λ"
                  (i, τ) <- parens typ
                  reserved "."
                  e <- expr
                  return $ Lam i τ e
              pi   = do
                  reserved "π"
                  (i, τ) <- parens typ
                  reserved "."
                  e <- expr
                  return $ Pi i τ e
              app  = do
                  e <- parens expr
                  ρs <- many $ parens expr
                  return $ foldl App e ρs
              typ  = do
                  v <- word
                  reserved ":"
                  τ <- expr
                  return $ (v, τ)
    

    -- parsing for REPL commands


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
