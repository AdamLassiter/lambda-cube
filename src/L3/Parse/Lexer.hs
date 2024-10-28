{-# LANGUAGE CPP #-}

-- | Lexer from Strings into Tokens
module L3.Parse.Lexer (Token (..), alternatives, comment, lexSrc, grammar) where

import Control.Applicative hiding (many)
import L3.Log
import L3.Parse.Parsec
import L3.Parse.Parser
import L3.Parse.StringParsec
import L3.Util

trace = traceU "Parse::Lexer"

-- | Lex strings into tokens, canonical form for the syntax definitition
data Token
  = OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | HasType
  | At
  | StarT
  | BoxT
  | Arrow
  | LambdaT
  | PiT
  | AutoT
  | Symbol String
  | Number Int
  | Comment String
  | EOL
  deriving (Show, Eq)

-- | A list of Alternatives that may be used to lex a string into tokens.
alternatives :: [Parser String Token]
alternatives =
  [ reserved "(" >> trace "open-paren" (pure OpenParen),
    reserved ")" >> trace "close-paren" (pure CloseParen),
    reserved "[" >> trace "open-bracket" (pure OpenBracket),
    reserved "]" >> trace "close-bracket" (pure CloseBracket),
    reserved "*" >> trace "star" (pure StarT),
    reserved "#" >> trace "box" (pure BoxT),
    reserved ":" >> trace "has-type" (pure HasType),
    reserved "." >> trace "arrow" (pure Arrow),
    reserved "→" >> trace "arrow" (pure Arrow),
    reserved "->" >> trace "arrow" (pure Arrow),
    reserved "λ" >> trace "lambda" (pure LambdaT),
    reserved "π" >> trace "pi" (pure PiT),
#ifdef SETNOTATION
    reserved "⊤" >> trace "star" (pure StarT),
    reserved "⊥" >> trace "box" (pure BoxT),
    reserved "∈" >> trace "has-type" (pure HasType),
    reserved "∃" >> trace "lambda" (pure LambdaT),
    reserved "∀" >> trace "pi" (pure PiT),
#endif
    reserved "@" >> trace "at" (pure At),
    reserved "--" >> trace "comment" comment,
    reserved "\n" >> trace "end-of-line" (pure EOL),
    Number <$> number,
    Symbol <$> word
  ]

-- | Parse a comment, taking characters until an End-Of-Line
comment :: Parser String Token
comment = trace "comment" $ do
  cs <- many $ satisfy (/= '\n')
  pure $ Comment cs

-- | Parse a string into canonical form using tokens
lexSrc :: String -> Result [Token]
lexSrc = runParser grammar

-- | The grammar for this parser is the collection of Alternatives
grammar :: Parser String [Token]
grammar = many $ foldl1 (<|>) alternatives
