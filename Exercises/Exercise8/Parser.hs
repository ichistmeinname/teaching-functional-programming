module Parser where

import Control.Monad

import Prelude hiding ((<*>),(<*),(*>),(<$>))

infixl 3 <|>
infixl 4 <*>,<*,*>

--import Parser

-- data Parser a

-- parse :: Parser a -> String -> Maybe a

-- char :: Char -> Parser ()

-- empty :: Parser ()

-- (*>) :: Parser a -> Parser b -> Parser b

-- (<*) :: Parser a -> Parser b -> Parser a
-- (<*) = flip (*>) ? No

-- (<|>) :: Parser a -> Parser a -> Parser a 

-- yield :: a -> Parser a

-- <*> :: Parser (a -> b) -> Parser a -> Parser b

(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> p = yield f <*> p 

nested :: Parser Int 
nested = (\n m -> max (n + 1) m)
     <$> (char '(' *> nested <* char ')') <*> nested
     <|> yield 0

aStar :: Parser Int
aStar = (+1) <$> (char 'a' *> aStar) <|> yield 0  

-- anyChar :: Parser Char 

-- check :: (a -> Bool) -> Parser a -> Parser a

-- failure :: Parser ()


--char :: Char -> Parser ()
--char c = check (c==) anyChar *> empty

-- many should not be applied to parser, which match the empty word
many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p
     <|> yield []


anbn :: Parser Int
anbn = (+1) <$> (char 'a' *> anbn <* char 'b')
   <|> char 'a' *> char 'b' *> yield 1

palindrom =
      (check (\ (v1,v2) -> v1 == reverse v2)
    $ (,)
    <$> many anyChar <* optional anyChar <*> many anyChar
     ) *> empty
optional :: Parser a -> Parser ()
optional p = p *> empty <|> empty

type Parser a = String -> Maybe (a,String)

anyChar :: Parser Char
anyChar "" = Nothing
anyChar (c:cs) = Just (c,cs)

char c ""      = Nothing
char c (c':cs) = if c==c' then Just ((),cs)
                          else Nothing 

p *> q = \s -> p s >>= \(_,s') -> q s'

p <* q = \s -> p s >>= \(r,s') ->
               q s' >>= \(_,s'') ->
               return (r,s'')

p <*> q = \s -> p s >>= \(f,s') ->
                q s' >>= \(a,s'') ->
                return (f a,s'')

parse :: Parser a -> String -> Maybe a
parse p s = case p s of
              Just (r,"") -> Just r
              _           -> Nothing

empty :: Parser ()
empty = \s -> Just ((),s)

yield :: a -> Parser a
yield x = \s -> Just (x,s)


check :: (a -> Bool) -> Parser a -> Parser a
check p q = \s -> q s >>= \(r,s') ->
                  guard (p r) >>
                  return (r,s')
 
p <|> q = \s -> p s `mplus` q s
