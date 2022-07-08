{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

-- | Parser from Tokens into Expressions
module L3.Parse.ExprParser (parseExpr) where

import Control.Applicative
  ( Alternative (empty, (<|>)),
    Applicative (pure),
    optional,
    (<$>),
  )
import Data.Maybe
import L3.Core
import L3.Log
import L3.Parse.Lexer
import L3.Parse.Parsec
import L3.Parse.Parser
import L3.Parse.TokenParsec
import L3.Util
import Prelude hiding (pi)

trace = traceU "Parse::ExprParser"

traceIO msgFn parser = Parser $ \i -> trace (msgFn i) (unParser parser i)

-- | Parse a string to a named expression (using string labels)
--  Strip comments here, as they apply inside any context and are difficult to deal with otherwise
parseExpr :: [Token] -> Result ShowExpr
parseExpr tks = trace ("parseExpr " ++ show tks) (parseExpr' tks)

parseExpr' tks = es
  where
    es = runParser sugarE . filter (not . ann) $ tks
    ann (Comment c) = True
    ann EOL = True
    ann _ = False

-- | Sugared expression, injection point for additional syntax niceties
--  S :: A | [π(_:A)].S
sugarE :: Parser [Token] ShowExpr
sugarE = traceIO (\i -> "sugarE " ++ show i) sugarE'

#ifdef ANONYMOUSPI
sugarE' = do
  ex <- appE
  anonPi <- optional (anonPiE ex)
  pure $ fromMaybe ex $ anonPi
#else
sugarE' = do appE
#endif

-- | Applicative expression, unknown and unbounded length
--  A :: F [app F ..]
appE :: Parser [Token] ShowExpr
appE = traceIO (\i -> "appE " ++ show i) appE'

appE' = do
  exprs <- some funE
  return $ foldl1 App exprs

-- | Function expression
--  F :: λ(s:τ).E | π(s:τ).E | T | (S)
funE :: Parser [Token] ShowExpr
funE = traceIO (\i -> "funE " ++ show i) funE'

#ifdef STRICTPARENS
funE' =
  lamE
    <|> piE
    <|> termE
    <|> braces sugarE
#else
funE' =
  lamE
    <|> piE
    <|> termE
    <|> parens sugarE
#endif

-- | Terminal expression, bounded in length and with no children
--  T :: * | # | n@v | v
termE :: Parser [Token] ShowExpr
termE = traceIO (\i -> "termE " ++ show i) termE'

termE' =
  star
    <|> box
    <|> nsVar
    <|> var

-- | Arrow expression, a component of functions
--  (->) :: (s:τ).
arrE :: Parser [Token] (Name, ShowExpr)
arrE = traceIO (\i -> "arrE " ++ show i) arrE'

#ifdef STRICTPARENS
arrE' = do
  (i, τ) <- brackets typE
  _ <- one Arrow
  pure (i, τ)
#else 
arrE' = do
  (i, τ) <- parens typE
  _ <- one Arrow
  pure (i, τ)
#endif

-- | Type expression, a symbol has type expr
--  τ :: s:S
typE :: Parser [Token] (Name, ShowExpr)
typE = traceIO (\i -> "typE " ++ show i) typE'

typE' = do
  t <- symbol
  case t of
    (Symbol s) -> do
      one HasType
      (Name s,) <$> sugarE
    _ -> empty

-- | Star axiom
--  * :: Star
star :: Parser [Token] ShowExpr
star = traceIO (\i -> "star " ++ show i) star'

star' = do
  one StarT
  pure Star

-- | Box axiom
--  # :: Box
box :: Parser [Token] ShowExpr
box = traceIO (\i -> "box " ++ show i) box'

box' = do
  one BoxT
  pure Box

-- | Variable axiom
--  s :: Var s
var :: Parser [Token] ShowExpr
var = traceIO (\i -> "var " ++ show i) var'

var' = do
  t <- symbol
  case t of
    (Symbol s) -> pure $ Var $ Name s
    _ -> empty

-- | Namespaced-variable axiom
--  n@s :: Var n@s
nsVar :: Parser [Token] ShowExpr
nsVar = traceIO (\i -> "nsVar " ++ show i) nsVar'

nsVar' = do
  ns <- symbol
  _ <- Left <$> one At
  t <- var <|> nsVar
  case (ns, t) of
    (Symbol n, Var (Name s)) -> pure $ Var $ Name $ n ++ "@" ++ s
    _ -> empty

-- | Lambda function
--  λ(s:S).S :: Lam s S S
lamE :: Parser [Token] ShowExpr
lamE = traceIO (\i -> "lamE " ++ show i) lamE'

lamE' = do
  _ <- one LambdaT
  (i, τ) <- arrE
  Lam i τ <$> sugarE

-- | Pi function
--  π(s:S).S :: Pi s S S
piE :: Parser [Token] ShowExpr
piE = traceIO (\i -> "piE " ++ show i) piE'

piE' = do
  _ <- one PiT
  (i, τ) <- arrE
  Pi i τ <$> sugarE

-- | Anonymous-pi function, where the name is unused and therefore not written
--  Note the implicit expression argument, as otherwise sugar = (app >> arrow >> sugar) <|> app
--  If this were not the case, all expressions are parsed as anonymous pis and will fail at the final arrow
--  This is an exponential slowdown
--  S :: π(_:S).S
anonPiE :: ShowExpr -> Parser [Token] ShowExpr
anonPiE τ = traceIO (\i -> "anonPiE " ++ show i) (anonPiE' τ)

anonPiE' τ = do
  _ <- one Arrow
  Pi (Name "_") τ <$> sugarE
