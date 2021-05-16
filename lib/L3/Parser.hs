{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- Reverse-parse of Pretty shows
module L3.Parser (module L3.Parser, module L3.Parsec) where
    import Prelude hiding (pi)
    import L3.Pretty
    import L3.Parsec
    import L3.Util

    import Control.Applicative hiding (some, many)

    -- parse a string to a named expression (using string labels)
    parseExpr :: String -> Result ShowExpr
    parseExpr = runParser aexpr

    -- applicative expression, infinite length
    -- A :: A app A
    aexpr :: Parser ShowExpr
    aexpr = do
       exprs <- many bexpr
       return $ foldl1 App exprs

    -- bounded expression, length linear with nesting
    -- B :: * | # | n | λF | ∀F | (A)
    bexpr :: Parser ShowExpr
    bexpr = star
        <|> box
        <|> var
        <|> lam
        <|> pi
        <|> parens aexpr

    -- function expression
    -- F :: (T) . B
    fexpr :: Parser (Name, ShowExpr, ShowExpr)
    fexpr = do
        (i, τ) <- parens texpr
        reserved "." <|> reserved "->" <|> reserved "→"
        (i, τ,) <$> aexpr

    -- type expression
    -- T :: n : A
    texpr :: Parser (Name, ShowExpr)
    texpr = do
        v <- word
        reserved ":"
        (Name v,) <$> aexpr

    star :: Parser ShowExpr
    star = do
        reserved "*"
        pure Star
    box :: Parser ShowExpr
    box = do
        reserved "#"
        pure Box
    var :: Parser ShowExpr
    var = do
        Var . Name <$> word
    lam :: Parser ShowExpr
    lam = do
        reserved "λ" <|> reserved "lambda " <|> reserved "\\"
        (i, τ, e) <- fexpr
        pure (Lam i τ e)
    pi :: Parser ShowExpr
    pi = do
        reserved "∀" <|> reserved "forall " <|> reserved "π"
        (i, τ, e) <- fexpr
        pure (Pi i τ e)
