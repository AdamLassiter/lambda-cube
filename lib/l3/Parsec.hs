-- Small parsec-like module
module L3.Parsec where
    import L3.Util

    import Data.Char
    import Control.Monad
    import Control.Applicative hiding (some, many)

    newtype Parser a = Parser { parse :: String -> [(a, String)] }


    runParser :: Parser a -> String -> Result a
    runParser m s =
        case parse m s of
            (res, []):[] -> Right res
            (_, rem):[]  -> throwError $ "parser failed to consume: " ++ rem
            _            -> throwError $ "parser error"

    item :: Parser Char
    item = Parser $ \s ->
        case s of
            []     -> []
            (c:cs) -> [(c, cs)]

    bind :: Parser a -> (a -> Parser b) -> Parser b
    bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

    unit :: a -> Parser a
    unit a = Parser (\s -> [(a,s)])

    instance Functor Parser where
        fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

    instance Applicative Parser where
        pure = return
        (Parser left) <*> (Parser right) = Parser (\s -> [(f a, s2) | (f, s1) <- left s, (a, s2) <- right s1])

    instance Monad Parser where
        return = unit
        (>>=)  = bind

    instance MonadPlus Parser where
        mzero = failure
        mplus = combine

    instance Alternative Parser where
        empty = mzero
        (<|>) = option

    combine :: Parser a -> Parser a -> Parser a
    combine p q = Parser (\s -> parse p s ++ parse q s)

    failure :: Parser a
    failure = Parser (\cs -> [])

    option :: Parser a -> Parser a -> Parser a
    option  p q = Parser $ \s ->
        case parse p s of
            []     -> parse q s
            res    -> res

    some :: (Alternative f) => f a -> f [a]
    some v = some_v
        where
            many_v = some_v <|> pure []
            some_v = (:) <$> v <*> many_v

    many :: (Alternative f) => f a -> f [a]
    many v = many_v
        where
            many_v = some_v <|> pure []
            some_v = (:) <$> v <*> many_v

    satisfy :: (Char -> Bool) -> Parser Char
    satisfy p = item `bind` \c ->
        if p c
        then unit c
        else (Parser (\cs -> []))

    oneOf :: [Char] -> Parser Char
    oneOf s = satisfy (flip elem s)

    chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
    chainl p op a = (p `chainl1` op) <|> return a

    chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
    p `chainl1` op = do {a <- p; rest a}
        where rest a = (do
                    f <- op
                    b <- p
                    rest (f a b))
                <|> return a

    char :: Char -> Parser Char
    char c = satisfy (c ==)

    letter :: Parser Char
    letter = satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'])

    natural :: Parser Integer
    natural = read <$> some (satisfy isDigit)

    string :: String -> Parser String
    string [] = return []
    string (c:cs) = do {char c; string cs; return (c:cs)}

    token :: Parser a -> Parser a
    token p = do {a <- p; spaces; return a}

    reserved :: String -> Parser String
    reserved s = token (string s)

    space :: Parser Char
    space = oneOf " \n\r"

    spaces :: Parser String
    spaces = many $ space

    digit :: Parser Char
    digit = satisfy isDigit

    number :: Parser Int
    number = do
        s <- string "-" <|> return []
        cs <- some digit
        return $ read (s ++ cs)
    
    word :: Parser String
    word = do
        cs <- some letter
        spaces
        return cs

    parens :: Parser a -> Parser a
    parens m = do
        reserved "("
        n <- m
        reserved ")"
        return n