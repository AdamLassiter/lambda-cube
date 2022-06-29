{-# LANGUAGE LambdaCase #-}

-- | Parsec utils for parsing Tokens into Tokens
module L3.Parse.TokenParsec where

import Control.Applicative (Alternative ((<|>)))
import L3.Parse.Lexer
  ( Token (CloseParen, CloseSquare, Number, OpenParen, OpenSquare, Symbol),
  )
import L3.Parse.Parsec
import L3.Parse.Parser

item :: Parser [Token] Token
item = Parser $ \case
  [] -> []
  (c : cs) -> [(c, cs)]

satisfy :: (Token -> Bool) -> Parser [Token] Token
satisfy p =
  item `bind` \c ->
    if p c
      then unit c
      else Parser (const [])

one :: Token -> Parser [Token] Token
one s = satisfy (== s)

oneOf :: [Token] -> Parser [Token] Token
oneOf s = satisfy (`elem` s)

token :: Parser [Token] a -> Parser [Token] a
token p = do p

reserved :: Token -> Parser [Token] Token
reserved s = token (one s)

number :: Parser [Token] Token
number = satisfy isNumber
  where
    isNumber (Number n) = True
    isNumber _ = False

symbol :: Parser [Token] Token
symbol = satisfy isSymbol
  where
    isSymbol (Symbol s) = True
    isSymbol _ = False

parens :: Parser [Token] a -> Parser [Token] a
parens m = (roundParens m) <|> (squareParens m)

squareParens :: Parser [Token] a -> Parser [Token] a
squareParens m = do
  _ <- reserved OpenSquare
  n <- m
  _ <- reserved CloseSquare
  return n

roundParens :: Parser [Token] a -> Parser [Token] a
roundParens m = do
  _ <- reserved OpenParen
  n <- m
  _ <- reserved CloseParen
  return n
