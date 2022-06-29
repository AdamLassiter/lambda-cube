module L3.Parse.Parser where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (..))

newtype Parser i o = Parser {parse :: i -> [(o, i)]}

bind :: Parser i o -> (o -> Parser i o') -> Parser i o'
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: o -> Parser i o
unit a = Parser (\s -> [(a, s)])

combine :: Parser i o -> Parser i o -> Parser i o
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser i o
failure = Parser (const [])

option :: Parser i o -> Parser i o -> Parser i o
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    res -> res

instance Functor (Parser i) where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative (Parser i) where
  pure = unit
  (Parser left) <*> (Parser right) = Parser (\s -> [(f a, s2) | (f, s1) <- left s, (a, s2) <- right s1])

instance Monad (Parser i) where
  (>>=) = bind

instance MonadPlus (Parser i) where
  mzero = failure
  mplus = combine

instance Alternative (Parser i) where
  empty = mzero
  (<|>) = option
