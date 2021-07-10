{-# LANGUAGE LambdaCase #-}

-- Small parsec-like module
module L3.Parsec (module L3.Parsec, module L3.Util) where
    import L3.Util

    import Data.Char
    import Control.Monad
    import Control.Applicative hiding (some, many)


    newtype Parser i o = Parser { parse :: i -> [(o, i)] }


    runParser :: (Alternative f, Show (f i), Show o, Eq (f i)) => Parser (f i) o -> f i -> Result o
    runParser m s =
        case parse m s of
            [(res, rem')] | rem' == empty  -> Right res
            [(res, rem')] -> throwError $ "parser did consume: " ++ show res ++ "\n but failed to consume: " ++ show rem'
            []           -> throwError $ "parser failed to consume anything from " ++ show s
            rs           -> throwError $ "parser produced multiple results " ++ show rs ++ " but this is not supported"

    bind :: Parser i o -> (o -> Parser i o') -> Parser i o'
    bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

    unit :: o -> Parser i o
    unit a = Parser (\s -> [(a,s)])

    instance Functor (Parser i) where
        fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

    instance Applicative (Parser i) where
        pure = return
        (Parser left) <*> (Parser right) = Parser (\s -> [(f a, s2) | (f, s1) <- left s, (a, s2) <- right s1])

    instance Monad (Parser i) where
        return = unit
        (>>=)  = bind

    instance MonadPlus (Parser i) where
        mzero = failure
        mplus = combine

    instance Alternative (Parser i) where
        empty = mzero
        (<|>) = option

    combine :: Parser i o -> Parser i o -> Parser i o
    combine p q = Parser (\s -> parse p s ++ parse q s)

    failure :: Parser i o
    failure = Parser (const [])

    option :: Parser i o -> Parser i o -> Parser i o
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

    chainl :: Parser i o -> Parser i (o -> o -> o) -> o -> Parser i o
    chainl p op a = (p `chainl1` op) <|> return a

    chainl1 :: Parser i o -> Parser i (o -> o -> o) -> Parser i o
    p `chainl1` op = do {a <- p; rest a}
        where rest a = (do
                    f <- op
                    b <- p
                    rest (f a b))
                <|> return a
