module Parser where

import Control.Applicative


newtype Parser a = Parser { runParser :: String -> [(a,String)] }

parse :: Parser a -> String -> Maybe a
parse p s = case filter (null . snd) $ runParser p s of
              ((x,_):_) -> Just x
              _         -> Nothing

failure :: Parser a
failure = Parser (\_ -> [])

yield :: a -> Parser a
yield x = Parser (\s -> [(x,s)])

epsilon :: Parser ()
epsilon = yield ()

anyChar :: Parser Char
anyChar = Parser (\s -> case s of
                          []     -> []
                          (c:cs) -> [(c,cs)])

check :: (a -> Bool) -> Parser a -> Parser a
check ok p = Parser (filter (ok . fst) . runParser p)

char :: Char -> Parser ()
char c = check (c==) anyChar *> yield ()

word :: String -> Parser ()
word []     = epsilon
word (c:cs) = char c *> word cs


{-
(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> p = Parser (map (\ (x,s) -> (f x,s)) . runParser p)
-}
--infixl 4 <*>, <*, *>

{-
(<*) :: Parser a -> Parser b -> Parser a
p <* q = (\x _ -> x) <$> p <*> q

(*>) :: Parser a -> Parser b -> Parser b
p *> q = (\_ y -> y) <$> p <*> q
-}

infixl 1 *>=

(*>=) :: Parser a -> (a -> Parser b) -> Parser b
p *>= f  = Parser (\s -> [ (y,s2) | (x,s1) <- runParser p s,
                                    (y,s2) <- runParser (f x) s1 ])

instance Applicative Parser where
  pure = yield
  --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p <*> q = Parser (\s -> [ (f x, s2) | (f,s1) <- runParser p s,
                                        (x,s2) <- runParser q s1 ])

instance Monad Parser where
  return = yield
  (>>=)  = (*>=)

{-
many :: Parser a -> Parser [a]
many p = some p <|> yield []

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p
-}

instance Functor Parser where
  fmap f p = Parser (map (\ (x,s) -> (f x,s)) . runParser p)

{-
class Applicative f => Alternative f where
  empty :: f a

  (<|>) :: f a -> f a -> f a
  
  some p = (:) <$> p <*> many p
  
  many p = some p <|> yield []
-}

-- optional :: Parser a -> Parser (Maybe a)
-- optional p = Just <$> p  <|> yield Nothing

instance Alternative Parser where

  --empty :: Parser ()
  empty = failure

  --(<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser (\s -> runParser p s ++ runParser q s)

-- Maybe style version of <|>, which allows you to cut of
-- alternatives, if the first parser matches.
(<!>) :: Parser a -> Parser a -> Parser a
p <!> q = Parser (\s -> case runParser p s of
                          [] -> runParser q s
                          xs -> xs)
