-- |Lexer from Strings into Tokens
module L3.Lexer (module L3.Lexer) where
    import L3.Logging
    import L3.StringParsec

    import Control.Applicative hiding (many)


    -- |Lex strings into tokens, canonical form for the syntax definitition
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

    debugLexer = debugU "Lexer"


    -- |A list of Alternatives that may be used to lex a string into tokens.
    alternatives :: [Parser String Token]
    alternatives = [ reserved "(" >> debugLexer "open-paren" (pure OpenParen)
                   , reserved ")" >> debugLexer "close-paren" (pure CloseParen)

                   , reserved "*" >> debugLexer "star" (pure StarT)
                   , reserved "⊤" >> debugLexer "star" (pure StarT)

                   , reserved "#" >> debugLexer "box" (pure BoxT)
                   , reserved "⊥" >> debugLexer "box" (pure BoxT)

                   , reserved ":" >> debugLexer "has-type" (pure HasType)
                   , reserved "∈" >> debugLexer "has-type" (pure HasType)

                   , reserved "."  >> debugLexer "arrow" (pure Arrow)
                   , reserved "→"  >> debugLexer "arrow" (pure Arrow)
                   , reserved "->" >> debugLexer "arrow" (pure Arrow)

                   , reserved "lambda" >> debugLexer "lambda" (pure LambdaT)
                   , reserved "∃"      >> debugLexer "lambda" (pure LambdaT)
                   , reserved "λ"      >> debugLexer "lambda" (pure LambdaT)

                   , reserved "forall" >> debugLexer "pi" (pure PiT)
                   , reserved "∀"      >> debugLexer "pi" (pure PiT)
                   , reserved "π"      >> debugLexer "pi" (pure PiT)

                   , reserved "@" >> debugLexer "at" (pure At)

                   , reserved "--" >> debugLexer "comment" comment
                   , reserved "\n" >> debugLexer "end-of-line" (pure EOL)

                   , Number <$> number
                   , Symbol <$> word
                   ]

    -- |Parse a comment, taking characters until an End-Of-Line
    comment :: Parser String Token
    comment = debugLexer "comment" $ do
        cs <- many $ satisfy (/= '\n')
        pure $ Comment cs

    -- |Parse a string into canonical form using tokens
    lexSrc :: String -> Result [Token]
    lexSrc = runParser grammar

    -- |The grammar for this parser is the collection of Alternatives
    grammar :: Parser String [Token]
    grammar = many $ foldl1 (<|>) alternatives
