{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- Parser from tokens into Expressions
module L3.Parser (module L3.Parser, module L3.Lexer, module L3.Pretty) where
    import Prelude hiding (pi)

    import L3.Pretty
    import L3.Lexer
    import L3.TokenParsec

    import Control.Applicative hiding (some, many)


    -- parse a string to a named expression (using string labels)
    parseExpr :: [Token] -> Result ShowExpr
    parseExpr = runParser appE . filter (not . annotation)
        where annotation (Comment c) = True
              annotation EOL         = True
              annotation _           = False

    -- applicative expression, infinite length
    -- A :: A app A
    appE :: Parser [Token] ShowExpr
    appE = do
        expr <- sugarE
        exprs <- many sugarE
        return $ foldl App expr exprs

    -- sugared expression, allows for anonymous pi types
    sugarE :: Parser [Token] ShowExpr
    sugarE = anonpiE
        <|> bndE

    -- bounded expression, length linear with nesting
    -- B :: * | # | n | λF | ∀F | (A)
    bndE :: Parser [Token] ShowExpr
    bndE = star
        <|> box
        <|> var
        <|> lamE
        <|> piE
        <|> parens appE


    -- function expression
    -- F :: (T) . B
    fnE :: Parser [Token] (Name, ShowExpr, ShowExpr)
    fnE = do
        (i, τ) <- parens typE
        _ <- one Arrow
        (i, τ,) <$> appE

    -- type expression
    -- T :: n : A
    typE :: Parser [Token] (Name, ShowExpr)
    typE = do
        t <- symbol
        case t of
          (Symbol s) -> do
              one HasType
              (Name s,) <$> appE
          _          -> empty

    star :: Parser [Token] ShowExpr
    star = do
        one StarT
        pure Star
    box :: Parser [Token] ShowExpr
    box = do
        one BoxT
        pure Box

    var :: Parser [Token] ShowExpr
    var = do
        t <- symbol
        case t of
          (Symbol s) -> pure $ Var $ Name s
          _          -> empty

    lamE :: Parser [Token] ShowExpr
    lamE = do
        _ <- one LambdaT
        (i, τ, e) <- fnE
        pure (Lam i τ e)

    piE :: Parser [Token] ShowExpr
    piE = do
        _ <- one PiT
        (i, τ, e) <- fnE
        pure (Pi i τ e)
    anonpiE :: Parser [Token] ShowExpr
    anonpiE = do
        τ <- var <|> parens (lamE <|> piE <|> anonpiE)
        _ <- one Arrow
        Pi (Name "_") τ <$> bndE
