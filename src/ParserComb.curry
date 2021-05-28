--- ----------------------------------------------------------------------------
--- This module provides some simple parser combinators.
---
--- @author  Jan Tikovsky
--- @version October 2017
--- ----------------------------------------------------------------------------
module ParserComb where

infixl 3 <|>
infixl 4 <*, *>, <$>, <*>

data Parser tok a = Parser { runParser :: [tok] -> Either String ([tok], a) }

instance Functor (Parser tok) where
  fmap f (Parser g) = Parser $ \ts -> case g ts of
                                        Left s -> Left s
                                        Right (ts', x) -> Right (ts', f x)

instance Applicative (Parser tok) where
  pure = return
  af <*> ax = af >>= \f -> fmap f ax

instance Monad (Parser tok) where
  return = yield

  (Parser p) >>= f = Parser $ \ts -> case p ts of
    Left         e -> Left e
    Right (ts', x) -> runParser (f x) ts'

yield :: a -> Parser tok a
yield x = Parser $ \ts -> Right (ts, x)

--- sequential application of two parsers keeping the result of the first one
(<*) :: Parser tok a -> Parser tok b -> Parser tok a
p <* q = const <$> p <*> q

--- sequential application of two parsers keeping the result of the second one
(*>) :: Parser tok a -> Parser tok b -> Parser tok b
p *> q = flip const <$> p <*> q

--- alternative application of two parsers
(<|>) :: Parser tok a -> Parser tok a -> Parser tok a
p <|> q = Parser $ \ts -> case (runParser p) ts of
  Left         _ -> (runParser q) ts
  Right (ts', r) -> Right (ts', r)

eof :: Parser tok a
eof = Parser $ \_ -> Left "Unexpected end-of-file"

unexpected :: Show tok => tok -> Parser tok a
unexpected t = Parser $ \_ -> Left $ "Unexpected token " ++ show t

terminal :: (Eq tok, Show tok) => tok -> Parser tok ()
terminal tok = Parser $ \tokens -> case tokens of
  []               -> (runParser eof) []
  t:ts | tok == t  -> Right (ts, ())
       | otherwise -> (runParser (unexpected t)) ts

--- 'star' operation on parsers
many :: Parser tok a -> Parser tok [a]
many p = some p <|> yield []

--- 'plus' operation on parsers
some :: Parser tok a -> Parser tok [a]
some p = (:) <$> p <*> many p

-- TODO: Remove when provided by Prelude

--- Apply a pure function to the result of a monadic action.
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) f act = act >>= \x -> return (f x)

--- Apply a function originating from the first monadic computation
--- to the result of the second monadic action.
(<*>) :: Monad m => m (a -> b) -> m a -> m b
a <*> b = a >>= \f -> b >>= \x -> return (f x)
