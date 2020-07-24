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
                    | ExprCtx String NamedExpr
                    | TypeCtx String NamedExpr
                    deriving (Show)


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
       <|> par
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
                  (i, τ) <- typ
                  reserved "."
                  e <- expr
                  return $ Lam i τ e
              pi = do
                  reserved "π"
                  (i, τ) <- typ
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
              par = do
                  e <- parens expr
                  return $ e
    

    -- parsing for REPL commands

    parseRepl :: String -> Result ReplAction
    parseRepl = runParser replAction
    
    replAction :: Parser ReplAction
    replAction = quit
             <|> evalOf
             <|> typeOf
             <|> printOf
             <|> letCtx
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
              letCtx = do
                  reserved "let"
                  name <- word
                  letAct <- exprCtx name <|> typeCtx name
                  return $ letAct
              exprCtx name = do
                  reserved "="
                  e <- expr
                  return $ ExprCtx name e
              typeCtx name = do
                  reserved ":"
                  e <- expr
                  return $ TypeCtx name e


    -- run REPL

    doAction :: ReplAction -> NamedCtx -> NamedCtx -> Result (NamedExpr, NamedCtx, NamedCtx)
    doAction None eCtx τCtx = Right (Var "", eCtx, τCtx)
    doAction Quit _ _       = throwError "quit"
    doAction (EvalOf expr) eCtx τCtx = mapR (\eval -> (eval, eCtx, τCtx)) (nNormalizeIn eCtx expr τCtx)
    doAction (TypeOf expr) eCtx τCtx = mapR (\eval -> (eval, eCtx, τCtx)) (nTypeIn eCtx expr τCtx)
    doAction (PrintOf expr) eCtx τCtx = Right (expr, eCtx, τCtx)
    doAction (TypeCtx name expr) eCtx τCtx = mapR (\τCtx'' -> (Var "", eCtx, τCtx'')) τCtx'
        where τCtx' = mapR (\(namedEval, _, _) -> τCtx ++ [(name, namedEval)]) (doAction (EvalOf expr) eCtx τCtx)
    doAction (ExprCtx name expr) eCtx τCtx = mapR (\eCtx'' -> (Var "", eCtx'', τCtx)) eCtx'
        where eCtx' = mapR (\(namedEval, _, _) -> eCtx ++ [(name, namedEval)]) (doAction (EvalOf expr) eCtx τCtx)

    main :: IO ()
    main = runInputT defaultSettings repl

    repl :: InputT IO ()
    repl = repl0 [] []
        where showE = showExpr id
              showC = showCtx id
              repl0 eCtx τCtx = do
                outputStrLn $ showC eCtx
                outputStrLn $ showC τCtx
                inp' <- getInputLine ">> "
                (eCtx', τCtx') <- case inp' of
                    Just input -> do
                        let act' = parseRepl input
                        case act' of
                            Right action -> do
                                case doAction action eCtx τCtx of
                                    Right (expr, eCtx', τCtx') -> do
                                        outputStrLn $ showE expr
                                        return (eCtx', τCtx')
                                    Left error -> do
                                        outputStrLn error
                                        return (eCtx, τCtx)
                            Left error -> do
                                outputStrLn error
                                return (eCtx, τCtx)
                    Nothing -> error "quit"
                repl0 eCtx' τCtx'
