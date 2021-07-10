{-# LANGUAGE LambdaCase #-}

module L3.TokenParsec (module L3.TokenParsec, module L3.Parsec) where
    import L3.Lexer
    import L3.Parsec

    item :: Parser [Token] Token
    item = Parser $ \case
          []     -> []
          (c:cs) -> [(c, cs)]

    satisfy :: (Token -> Bool) -> Parser [Token] Token
    satisfy p = item `bind` \c ->
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
        where isNumber (Number n) = True
              isNumber _          = False

    symbol :: Parser [Token] Token
    symbol = satisfy isSymbol
        where isSymbol (Symbol s) = True
              isSymbol _          = False

    parens :: Parser [Token] a -> Parser [Token] a
    parens m = do
        _ <- reserved OpenParen
        n <- m
        _ <- reserved CloseParen
        return n
