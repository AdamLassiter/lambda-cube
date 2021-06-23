{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- Reverse-parse of Pretty shows
module L3.Parser (module L3.Parser, module L3.Parsec) where
    import Prelude hiding (pi)
    import L3.Pretty
    import L3.Parsec

    import Control.Applicative hiding (some, many)

    -- parse a string to a named expression (using string labels)
    parseExpr :: String -> Result ShowExpr
    parseExpr = runParser appE

    -- applicative expression, infinite length
    -- A :: A app A
    appE :: Parser ShowExpr
    appE = do
        expr <- sugarE
        exprs <- many sugarE
        return $ foldl App expr exprs

    -- sugared expression, allows for anonymous pi types
    sugarE :: Parser ShowExpr
    sugarE = anonpiE
        <|> bndE

    -- bounded expression, length linear with nesting
    -- B :: * | # | n | λF | ∀F | (A)
    bndE :: Parser ShowExpr
    bndE = star
        <|> box
        <|> var
        <|> lamE
        <|> piE
        <|> parens appE


    fn :: Parser String
    fn = reserved "." <|> reserved "->" <|> reserved "→"
    -- function expression
    -- F :: (T) . B
    fnE :: Parser (Name, ShowExpr, ShowExpr)
    fnE = do
        (i, τ) <- parens typE
        _ <- fn
        (i, τ,) <$> appE

    -- type expression
    -- T :: n : A
    typE :: Parser (Name, ShowExpr)
    typE = do
        v <- word
        reserved ":"
        (Name v,) <$> appE

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

    lam :: Parser String
    lam = reserved "λ" <|> reserved "lambda " <|> reserved "\\"
    lamE :: Parser ShowExpr
    lamE = do
        _ <- lam
        (i, τ, e) <- fnE
        pure (Lam i τ e)

    pi :: Parser String
    pi = reserved "∀" <|> reserved "forall " <|> reserved "π"
    piE :: Parser ShowExpr
    piE = do
        _ <- pi
        (i, τ, e) <- fnE
        pure (Pi i τ e)
    anonpiE :: Parser ShowExpr
    anonpiE = do
        τ <- var <|> parens (lamE <|> piE <|> anonpiE)
        _ <- fn
        Pi (Name "_") τ <$> bndE
