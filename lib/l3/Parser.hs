-- Reverse-parse of Pretty shows
module L3.Parser (module L3.Parser, module L3.Parsec) where
    import L3.Pretty
    import L3.Parsec
    import L3.Util

    import Control.Applicative hiding (some, many)


    -- parse a string to a named expression (using string labels)
    parseExpr :: String -> Result ShowExpr
    parseExpr = runParser expr

    expr :: Parser ShowExpr
    expr = do
        es <- many expr0
        return $ foldl1 App es

    -- parser for calculus expressions
    expr0 :: Parser ShowExpr
    expr0 = star
       <|> box
       <|> var
       <|> lam
       <|> pi
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
                  reserved "λ" <|> reserved "\\"
                  (i, τ) <- typ
                  reserved "."
                  e <- expr
                  return $ Lam i τ e
              pi = do
                  reserved "∀" <|> reserved "forall"
                  (i, τ) <- typ
                  reserved "." <|> reserved "->"
                  e <- expr
                  return $ Pi i τ e
              typ = do
                  v <- word
                  reserved ":"
                  τ <- expr
                  return $ (v, τ)
              par = do
                  e <- parens expr
                  return $ e

    eval :: String -> (Result ShowExpr, Result ShowExpr)
    eval inp = (typ, expr)
        where inpExpr = parseExpr inp
              dbExpr = mapR fst $ fmapR (\ex -> index ex []) inpExpr
              normExpr = mapR normalize dbExpr
              dbType = fmapR (\dbEx -> inferType [] dbEx) normExpr
              typ = mapR (\dbTy -> named dbTy [] []) dbType
              expr = mapR (\ex -> named ex [] []) normExpr
