module Repl where
    import Core
    import Pretty
    import Parsec
    import Util

    import Control.Applicative hiding (some, many)
    import System.Console.Haskeline

    data ReplAction = None
                    | Quit
                    | EvalOf NamedExpr
                    | TypeOf NamedExpr
                    | PrintOf NamedExpr
                    | TypeCtx String NamedExpr


    -- parsing equivalent to Pretty shows

    parseExpr :: String -> Result NamedExpr
    parseExpr = runParser expr

    expr :: Parser NamedExpr
    expr = star
       <|> box
       <|> var
       <|> lam
       <|> pi
       <|> app
        where star = do
                  reserved "*"
                  return $ Star
              box = do
                  reserved "#"
                  return $ Box
              var = do
                  v <- word
                  return $ Var v
              lam = do
                  reserved "λ"
                  (i, τ) <- parens typ
                  reserved "."
                  e <- expr
                  return $ Lam i τ e
              pi = do
                  reserved "π"
                  (i, τ) <- parens typ
                  reserved "."
                  e <- expr
                  return $ Pi i τ e
              app = do
                  e <- parens expr
                  ρs <- many $ parens expr
                  return $ foldl App e ρs
              typ = do
                  v <- word
                  reserved ":"
                  τ <- expr
                  return $ (v, τ)
    

    -- parsing for REPL commands

    parseRepl :: String -> Result ReplAction
    parseRepl = runParser replAction
    
    replAction :: Parser ReplAction
    replAction = quit
             <|> evalOf
             <|> typeOf
             <|> printOf
             <|> typeCtx
             <|> none
        where none = do
                  spaces
                  return $ None
              quit = do
                  reserved "quit"
                  return $ Quit
              evalOf = do
                  reserved "eval"
                  e <- expr
                  return $ EvalOf e
              typeOf = do
                  reserved "type"
                  e <- expr
                  return $ TypeOf e
              printOf = do
                  reserved "print"
                  e <- expr
                  return $ PrintOf e
              typeCtx = do
                  reserved "let"
                  name <- word
                  reserved ":"
                  e <- expr
                  return $ TypeCtx name e


    -- run REPL

    doAction :: ReplAction -> NamedCtx -> (String, NamedCtx)
    doAction None ctx' = ("", ctx')
    doAction Quit ctx' = error "quit"
    doAction (EvalOf expr') ctx' = doAction (PrintOf namedEval) ctx'
        where (expr, ctx) = index expr' ctx'
              eval = normalize expr
              namedEval = named eval ctx ctx'
    doAction (TypeOf expr') ctx' = case (typeIn ctx eval) of
                                        Left err  -> ("type error: " ++ err, ctx')
                                        Right inf -> doAction (PrintOf $ named inf ctx ctx') ctx'
        where (expr, ctx) = index expr' ctx'
              eval = normalize expr
    doAction (PrintOf expr') ctx' = (showExpr id expr', ctx')
    doAction (TypeCtx name expr') ctx' = ("", ctx'')
        where (expr, ctx) = index expr' ctx'
              eval = normalize expr
              namedEval = named eval ctx ctx'
              ctx'' = ctx' ++ [(name, namedEval)]

    main :: IO ()
    main = runInputT defaultSettings repl

    repl :: InputT IO ()
    repl = repl0 []
        where showE = showExpr id
              showC = showCtx id
              repl0 ctx' = do
                inp' <- getInputLine ">> "
                ctx'' <- case inp' of
                    Just input -> do
                        let act' = parseRepl input
                        case act' of
                            Right action -> do
                                let (msg, ctx'') = doAction action ctx'
                                if msg /= ""
                                    then outputStrLn msg
                                    else return ()
                                return ctx''
                            Left error -> do
                                outputStrLn error
                                return ctx'
                    Nothing -> error "quit"
                repl0 ctx''
