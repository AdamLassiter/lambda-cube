{-# LANGUAGE OverloadedStrings #-}

-- Reverse-parse of Pretty shows
module L3.Parser (module L3.Parser, module L3.Parsec) where
    import Prelude hiding (pi)
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

    star :: Parser ShowExpr
    star = do
        reserved "*"
        return Star
    box :: Parser ShowExpr
    box = do
        reserved "#"
        return Box
    var :: Parser ShowExpr
    var = do
        Var . Name <$> word
    lam :: Parser ShowExpr
    lam = do
        reserved "λ" <|> reserved "\\"
        (i, τ) <- typ
        reserved "."
        Lam i τ <$> expr
    pi :: Parser ShowExpr
    pi = do
        reserved "∀" <|> reserved "forall"
        (i, τ) <- typ
        reserved "." <|> reserved "->"
        Pi i τ <$> expr
    par :: Parser ShowExpr
    par = do
        parens expr
    typ = do
        v <- word
        reserved ":"
        τ <- expr
        return (Name v, τ)

    eval :: String -> (Result ShowExpr, Result ShowExpr)
    eval inp = (typ, normExpr)
        where inpExpr = parseExpr inp
              normExpr = mapR normalize inpExpr
              typ = fmapR (inferType []) normExpr
