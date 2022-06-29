-- | Lexer from Strings into Tokens
module L3.Parse.Lexer (Token (..), alternatives, comment, lexSrc, grammar) where

import Control.Applicative hiding (many)
import L3.Log
import L3.Parse.Parsec
import L3.Parse.Parser
import L3.Parse.StringParsec
import L3.Util

debug = debugU "Parse::Lexer"

-- | Lex strings into tokens, canonical form for the syntax definitition
data Token
  = OpenParen
  | CloseParen
  | OpenSquare
  | CloseSquare
  | HasType
  | At
  | StarT
  | BoxT
  | Arrow
  | LambdaT
  | PiT
  | Symbol String
  | Number Int
  | Comment String
  | EOL
  deriving (Show, Eq)

-- | A list of Alternatives that may be used to lex a string into tokens.
alternatives :: [Parser String Token]
alternatives =
  [ reserved "(" >> debug "open-paren" (pure OpenParen),
    reserved ")" >> debug "close-paren" (pure CloseParen),
    reserved "[" >> debug "open-paren" (pure OpenSquare),
    reserved "]" >> debug "close-paren" (pure CloseSquare),
    reserved "*" >> debug "star" (pure StarT),
    reserved "⊤" >> debug "star" (pure StarT),
    reserved "#" >> debug "box" (pure BoxT),
    reserved "⊥" >> debug "box" (pure BoxT),
    reserved ":" >> debug "has-type" (pure HasType),
    reserved "∈" >> debug "has-type" (pure HasType),
    reserved "." >> debug "arrow" (pure Arrow),
    reserved "→" >> debug "arrow" (pure Arrow),
    reserved "->" >> debug "arrow" (pure Arrow),
    reserved "lambda" >> debug "lambda" (pure LambdaT),
    reserved "∃" >> debug "lambda" (pure LambdaT),
    reserved "λ" >> debug "lambda" (pure LambdaT),
    reserved "forall" >> debug "pi" (pure PiT),
    reserved "∀" >> debug "pi" (pure PiT),
    reserved "π" >> debug "pi" (pure PiT),
    reserved "@" >> debug "at" (pure At),
    reserved "--" >> debug "comment" comment,
    reserved "\n" >> debug "end-of-line" (pure EOL),
    Number <$> number,
    Symbol <$> word
  ]

-- | Parse a comment, taking characters until an End-Of-Line
comment :: Parser String Token
comment = debug "comment" $ do
  cs <- many $ satisfy (/= '\n')
  pure $ Comment cs

-- | Parse a string into canonical form using tokens
lexSrc :: String -> Result [Token]
lexSrc = runParser grammar

-- | The grammar for this parser is the collection of Alternatives
grammar :: Parser String [Token]
grammar = many $ foldl1 (<|>) alternatives
