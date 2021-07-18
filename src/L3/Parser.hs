{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Parser from Tokens into Expressions
module L3.Parser (module L3.Parser, module L3.Lexer, module L3.Core) where
    import Prelude hiding (pi)

    import L3.Core
    import L3.Lexer
    import L3.TokenParsec

    import Control.Applicative hiding (some, many)
    import Debug.Trace


    -- | Parse a string to a named expression (using string labels)
    -- | Strip comments here, as they apply inside any context and are difficult to deal with otherwise
    parseExpr :: [Token] -> Result ShowExpr
    parseExpr tks = mapR (foldl1 App) es
        where es = runParser sugarE . filter (not . ann) $ tks
              ann (Comment c) = True
              ann EOL         = True
              ann _           = False

    -- | Sugared expression, injection point for additional syntax niceties
    -- | S :: [π(_:]A[)].S  | A
    sugarE :: Parser [Token] ShowExpr
    sugarE = anonPiE <|> appE

    -- | Applicative expression, unknown and unbounded length
    -- | A :: F [app F ..]
    appE :: Parser [Token] ShowExpr
    appE = do
        expr <- funE
        exprs <- many funE
        return $ foldl App expr exprs

    -- | Function expression
    -- | F :: λ(s:τ).E | π(s:τ).E | T | (S)
    funE :: Parser [Token] ShowExpr
    funE = lamE
       <|> piE
       <|> termE
       <|> parens sugarE

    -- | Terminal expression, bounded in length and with no children
    -- | T :: * | # | n@v | v
    termE :: Parser [Token] ShowExpr
    termE = star
        <|> box
        <|> nsVar <|> var

    -- | Arrow expression, a component of functions
    -- | (->) :: (s:τ).
    arrE :: Parser [Token] (Name, ShowExpr)
    arrE = do
        (i, τ) <- parens typE
        _ <- one Arrow
        pure (i, τ)

    -- | Type expression, a symbol has type expr
    -- | τ :: s:S
    typE :: Parser [Token] (Name, ShowExpr)
    typE = do
        t <- symbol
        case t of
          (Symbol s) -> do
            one HasType
            (Name s,) <$> sugarE
          _          -> empty


    -- | Star axiom
    -- | * :: Star
    star :: Parser [Token] ShowExpr
    star = do
        one StarT
        pure Star

    -- | Box axiom
    -- | # :: Box
    box :: Parser [Token] ShowExpr
    box = do
        one BoxT
        pure Box

    -- | Variable axiom
    -- | s :: Var s
    var :: Parser [Token] ShowExpr
    var = do
        t <- symbol
        case t of
          (Symbol s) -> pure $ Var $ Name s
          _          -> empty
    -- | Namespaced-variable axiom
    -- | n@s :: Var n@s
    nsVar :: Parser [Token] ShowExpr
    nsVar = do
        ns <- symbol
        _ <- Left <$> one At
        t <- var <|> nsVar
        case (ns, t) of
          (Symbol n, Var (Name s)) -> pure $ Var $ Name $ n ++ "@" ++ s
          _                        -> empty

    -- | Lambda function
    -- | λ(s:S).S :: Lam s S S
    lamE :: Parser [Token] ShowExpr
    lamE = do
        _ <- one LambdaT
        (i, τ) <- arrE
        Lam i τ <$> sugarE

    -- | Pi function
    -- | π(s:S).S :: Lam s S S
    piE :: Parser [Token] ShowExpr
    piE = do
        _ <- one PiT
        (i, τ) <- arrE
        Pi i τ <$> sugarE

    -- | Anonymous-pi function, where the name is unused and therefore not written
    -- | S. :: π(_:S).S
    anonPiE :: Parser [Token] ShowExpr
    anonPiE = do
        τ <- appE
        _ <- one Arrow
        Pi (Name "_") τ <$> sugarE
