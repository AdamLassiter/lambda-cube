-- |Small parsec-like module
module L3.Parsec (module L3.Parsec, module L3.Util) where
    import L3.Logging
    import L3.Util

    import Data.Char
    import Control.Monad
    import Control.Applicative hiding (some, many)


    newtype Parser i o = Parser { parse :: i -> [(o, i)] }

    unParser :: Parser i o -> (i -> [(o, i)])
    unParser (Parser fn) = fn


    runParser :: (Alternative f, Show (f i), Show o, Eq (f i)) => Parser (f i) o -> f i -> Result o
    runParser m s =
        case parse m s of
            [(res, rem')] | rem' == empty  -> Right res
            [(res, rem')] -> Left $ throwError ["parser did consume:", showIdent res, "but failed to consume:", showIdent rem']
            []            -> Left $ throwError ["parser failed to consume anything from:", showIdent s]
            rs            -> Left $ throwError ("parser produced multiple results:": map showIdent rs)

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

    instance Alternative (Parser i) where
        empty = mzero
        (<|>) = option

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
