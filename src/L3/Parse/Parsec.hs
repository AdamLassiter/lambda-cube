-- | Small parsec-like module
module L3.Parse.Parsec where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import L3.Log.Logging
import L3.Parse.Parser
import L3.Util

unParser :: Parser i o -> (i -> [(o, i)])
unParser (Parser fn) = fn

runParser :: (Alternative f, Show (f i), Show o, Eq (f i)) => Parser (f i) o -> f i -> Result o
runParser m s =
  case parse m s of
    [(res, rem')] | rem' == empty -> Right res
    [(res, rem')] -> Left $ throwError ["parser did consume:", showIdent res, "but failed to consume:", showIdent rem']
    [] -> Left $ throwError ["parser failed to consume anything from:", showIdent s]
    rs -> Left $ throwError ("parser produced multiple results:" : map showIdent rs)

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
p `chainl1` op = do a <- p; rest a
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        <|> return a
