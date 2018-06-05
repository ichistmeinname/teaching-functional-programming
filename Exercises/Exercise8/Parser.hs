module Parser where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), guard)

newtype Parser a = Parser { parser :: String -> [(a, String)] }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> (\(x, s') -> (f x, s')) <$> p s 

instance Applicative Parser where
    pure x = Parser $ \s -> pure (x, s) 

    Parser fp <*> Parser p = 
        Parser $ \s -> do
            (f, s')   <- fp s
            (x , s'') <- p s'
            return (f x, s'') 
    
    Parser p *> Parser q = 
        Parser $ \s -> do
            (_, s') <- p s
            q s'

    Parser p <* Parser q = 
        Parser $ \s -> do
            (x, s')  <- p s 
            (_, s'') <- q s'
            return (x, s'')

instance Alternative Parser where
    empty = Parser (const []) 

    Parser p <|> Parser q = Parser $ \s -> p s <|> q s

    many p = some p <|> pure []

    some p = (:) <$> p <*> many p

instance Monad Parser where
    return = pure

    Parser p >>= f = 
        Parser $ \s -> do 
            (x, s') <- p s
            let Parser q = f x
            q s'

instance MonadPlus Parser where
    mplus = (<|>)
    mzero = empty

check :: (a -> Bool) -> Parser a -> Parser a
check pred (Parser p) = 
    Parser $ \s -> do
    (x, s') <- p s
    guard (pred x)
    return (x, s')

yield :: a -> Parser a 
yield = pure

parse :: Monad m => Parser a -> String -> m a
parse p s = 
    case dropWhile (not . null . snd) $ parser p s of
        (x ,_):_ -> return x
        _        -> fail "Parse error."

char :: Char -> Parser ()
char c = Parser $ \s ->
    case s of
        x:xs | c == x -> pure ((),xs)
        _             -> empty

anyChar :: Parser Char
anyChar = Parser $ \s ->
    case s of
        c:cs -> pure (c,cs)
        _    -> empty 

word :: String -> Parser String
word cs = foldr (\c cs' -> (:) <$>
                  ((anyChar >>= guard . (== c)) *> return c) <*> cs')
                (yield [])
                cs
