-- Lexer from Strings into Tokens
module L3.Lexer (module L3.Lexer) where
    import L3.StringParsec

    import Control.Applicative hiding (many)


    data Token = OpenParen
               | CloseParen
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


    -- A list of Alternatives that may be used to lex a string into tokens.
    alternatives :: [Parser String Token]
    alternatives = [
        reserved "(" >> pure OpenParen,
        reserved ")" >> pure CloseParen,

        reserved "*" >> pure StarT,

        reserved "#" >> pure BoxT,
        reserved "∎" >> pure BoxT,

        reserved ":" >> pure HasType,
        reserved "∈" >> pure HasType,

        reserved "." >> pure Arrow,
        reserved "->" >> pure Arrow,
        reserved "→" >> pure Arrow,

        reserved "lambda" >> pure LambdaT,
        reserved "∃" >> pure LambdaT,
        reserved "λ" >> pure LambdaT,

        reserved "forall" >> pure PiT,
        reserved "∀" >> pure PiT,
        reserved "π" >> pure PiT,

        reserved "@" >> pure At,

        Number <$> number,
        Symbol <$> word,

        do
          reserved "--"
          cs <- many $ satisfy (/= '\n')
          pure $ Comment cs,
        reserved "\n" >> pure EOL
      ]

    -- Parse a string into canonical form using tokens
    lexSrc :: String -> Result [Token]
    lexSrc = runParser grammar

    -- The grammar for this parser is the collection of Alternatives
    grammar :: Parser String [Token]
    grammar = many $ foldl1 (<|>) alternatives
