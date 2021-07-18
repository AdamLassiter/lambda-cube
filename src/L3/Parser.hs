{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- Parser from tokens into Expressions
module L3.Parser (module L3.Parser, module L3.Lexer, module L3.Core) where
    import Prelude hiding (pi)

    import L3.Core
    import L3.Lexer
    import L3.TokenParsec

    import Control.Applicative hiding (some, many)
    import Debug.Trace


    -- parse a string to a named expression (using string labels)
    parseExpr :: [Token] -> Result ShowExpr
    parseExpr tks = mapR (foldl1 App) es
        where es = runParser (some funE) . filter (not . ann) $ tks
              ann (Comment c) = True
              ann EOL         = True
              ann _           = False

    -- sugared expression
    sugarE :: Parser [Token] ShowExpr
    sugarE = anonPiE <|> appE

    -- applicative expression, infinite length
    -- A :: A app A
    appE :: Parser [Token] ShowExpr
    appE = do
        expr <- funE
        exprs <- many funE
        return $ foldl App expr exprs

    funE :: Parser [Token] ShowExpr
    funE = lamE
       <|> piE
       <|> termE
       <|> parens sugarE

    termE :: Parser [Token] ShowExpr
    termE = star
        <|> box
        <|> nsVar <|> var

    arrE :: Parser [Token] (Name, ShowExpr)
    arrE = do
        (i, τ) <- parens typE
        _ <- one Arrow
        pure (i, τ)

    -- type expression
    -- T :: n : A
    typE :: Parser [Token] (Name, ShowExpr)
    typE = do
        t <- symbol
        case t of
          (Symbol s) -> do
            one HasType
            (Name s,) <$> sugarE
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
    nsVar :: Parser [Token] ShowExpr
    nsVar = do
        ns <- symbol
        _ <- Left <$> one At
        t <- var <|> nsVar
        case (ns, t) of
          (Symbol n, Var (Name s)) -> pure $ Var $ Name $ n ++ "@" ++ s
          _                        -> empty

    lamE :: Parser [Token] ShowExpr
    lamE = do
        _ <- one LambdaT
        (i, τ) <- arrE
        Lam i τ <$> sugarE

    piE :: Parser [Token] ShowExpr
    piE = do
        _ <- one PiT
        (i, τ) <- arrE
        Pi i τ <$> sugarE

    anonPiE :: Parser [Token] ShowExpr
    anonPiE = do
        τ <- appE
        _ <- one Arrow
        Pi (Name "_") τ <$> sugarE
