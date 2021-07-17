{-# LANGUAGE LambdaCase #-}

module L3.StringParsec (module L3.StringParsec, module L3.Parsec) where
    import L3.Parsec

    import Control.Applicative hiding (many, some)
    import Data.Char (isDigit)

    item :: Parser String Char
    item = Parser $ \case
          []     -> []
          (c:cs) -> [(c, cs)]

    satisfy :: (Char -> Bool) -> Parser String Char
    satisfy p = item `bind` \c ->
        if p c
        then unit c
        else Parser (const [])

    oneOf :: String -> Parser String Char
    oneOf s = satisfy (`elem` s)

    char :: Char -> Parser String Char
    char c = satisfy (c ==)

    letter :: Parser String Char
    letter = satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['α'..'ω'] ++ ['Α'..'Ω'] ++ "_")

    natural :: Parser String Integer
    natural = read <$> some (satisfy isDigit)

    string :: String -> Parser String String
    string [] = return []
    string (c:cs) = do
        _ <- char c
        _ <- string cs
        return (c:cs)

    token :: Parser String a -> Parser String a
    token p = do
        a <- p
        _ <- spaces
        return a

    reserved :: String -> Parser String String
    reserved s = token (string s)

    space :: Parser String Char
    space = oneOf " \n\r"

    spaces :: Parser String String
    spaces = many space

    digit :: Parser String Char
    digit = satisfy isDigit

    number :: Parser String Int
    number = do
        s <- string "-" <|> return []
        cs <- some digit
        _ <- spaces
        return $ read (s ++ cs)

    word :: Parser String String
    word = do
        cs <- some letter
        _ <- spaces
        return cs

    parens :: Parser String a -> Parser String a
    parens m = do
        _ <- reserved "("
        n <- m
        _ <- reserved ")"
        return n
