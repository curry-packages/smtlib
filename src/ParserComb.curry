--- ----------------------------------------------------------------------------
--- This module provides some simple parser combinators.
---
--- @author  Jan Tikovsky
--- @version October 2017
--- ----------------------------------------------------------------------------
module ParserComb where

data Parser tok a = Parser { runParser :: [tok] -> Either String ([tok], a) }

instance Alternative (Parser tok) where
  empty = Parser $ const (Left "empty parser")
  l <|> r = Parser $ \ts -> case runParser l ts of
                              Left         _ -> runParser r ts
                              Right (ts', x) -> Right (ts', x)

instance Functor (Parser tok) where
  fmap f (Parser g) = Parser $ \ts -> case g ts of
                                        Left s -> Left s
                                        Right (ts', x) -> Right (ts', f x)

instance Applicative (Parser tok) where
  pure = return
  af <*> ax = af >>= \f -> fmap f ax

instance Monad (Parser tok) where
  return x = Parser $ \ts -> Right (ts, x)

  (Parser p) >>= f = Parser $ \ts -> case p ts of
    Left         e -> Left e
    Right (ts', x) -> runParser (f x) ts'

eof :: Parser tok a
eof = Parser $ \_ -> Left "Unexpected end-of-file"

unexpected :: Show tok => tok -> Parser tok a
unexpected t = Parser $ \_ -> Left $ "Unexpected token " ++ show t

terminal :: (Eq tok, Show tok) => tok -> Parser tok ()
terminal tok = Parser $ \tokens -> case tokens of
  []               -> (runParser eof) []
  t:ts | tok == t  -> Right (ts, ())
       | otherwise -> (runParser (unexpected t)) ts