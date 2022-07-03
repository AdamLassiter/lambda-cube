{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Parser from Tokens into Expressions
module L3.Parse.ExprParser (parseExpr) where

import Control.Applicative
  ( Alternative (empty, (<|>)),
    Applicative (pure),
    optional,
    (<$>),
  )
import Data.Maybe
import Debug.Trace
import L3.Core
import L3.Log
import L3.Parse.Lexer
import L3.Parse.Parsec
import L3.Parse.Parser
import L3.Parse.TokenParsec
import L3.Util
import Prelude hiding (pi)

trace = traceU "Parse::ExprParser"

debugIO msgFn parser = Parser $ \i -> trace (msgFn i) (unParser parser i)

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
--  S :: [π(_:]A[)].S  | A
sugarE :: Parser [Token] ShowExpr
sugarE = debugIO (\i -> "sugarE " ++ show i) sugarE'

sugarE' = do
  ex <- appE
  anonPi <- optional (anonPiE ex)
  pure $ fromMaybe ex anonPi

-- | Applicative expression, unknown and unbounded length
--  A :: F [app F ..]
appE :: Parser [Token] ShowExpr
appE = debugIO (\i -> "appE " ++ show i) appE'

appE' = do
  exprs <- some funE
  return $ foldl1 App exprs

-- | Function expression
--  F :: λ(s:τ).E | π(s:τ).E | T | (S)
funE :: Parser [Token] ShowExpr
funE = debugIO (\i -> "funE " ++ show i) funE'

funE' =
  lamE
    <|> piE
    <|> termE
    <|> parens sugarE

-- | Terminal expression, bounded in length and with no children
--  T :: * | # | n@v | v
termE :: Parser [Token] ShowExpr
termE = debugIO (\i -> "termE " ++ show i) termE'

termE' =
  star
    <|> box
    <|> nsVar
    <|> var

-- | Arrow expression, a component of functions
--  (->) :: (s:τ).
arrE :: Parser [Token] (Name, ShowExpr)
arrE = debugIO (\i -> "arrE " ++ show i) arrE'

arrE' = do
  (i, τ) <- parens typE
  _ <- one Arrow
  pure (i, τ)

-- | Type expression, a symbol has type expr
--  τ :: s:S
typE :: Parser [Token] (Name, ShowExpr)
typE = debugIO (\i -> "typE " ++ show i) typE'

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
star = debugIO (\i -> "star " ++ show i) star'

star' = do
  one StarT
  pure Star

-- | Box axiom
--  # :: Box
box :: Parser [Token] ShowExpr
box = debugIO (\i -> "box " ++ show i) box'

box' = do
  one BoxT
  pure Box

-- | Variable axiom
--  s :: Var s
var :: Parser [Token] ShowExpr
var = debugIO (\i -> "var " ++ show i) var'

var' = do
  t <- symbol
  case t of
    (Symbol s) -> pure $ Var $ Name s
    _ -> empty

-- | Namespaced-variable axiom
--  n@s :: Var n@s
nsVar :: Parser [Token] ShowExpr
nsVar = debugIO (\i -> "nsVar " ++ show i) nsVar'

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
lamE = debugIO (\i -> "lamE " ++ show i) lamE'

lamE' = do
  _ <- one LambdaT
  (i, τ) <- arrE
  Lam i τ <$> sugarE

-- | Pi function
--  π(s:S).S :: Lam s S S
piE :: Parser [Token] ShowExpr
piE = debugIO (\i -> "piE " ++ show i) piE'

piE' = do
  _ <- one PiT
  (i, τ) <- arrE
  Pi i τ <$> sugarE

-- | Anonymous-pi function, where the name is unused and therefore not written
--  Note the implicit expression argument, as otherwise sugar = (app >> arrow >> sugar) <|> app
--  If this were not the case, all expressions are parsed as anonymous pis and will fail at the final arrow
--  This is an exponential slowdown
--  S. :: π(_:S).S
anonPiE :: ShowExpr -> Parser [Token] ShowExpr
anonPiE τ = debugIO (\i -> "anonPiE " ++ show i) (anonPiE' τ)

anonPiE' τ = do
  -- τ <- appE
  _ <- one Arrow
  Pi (Name "_") τ <$> sugarE
