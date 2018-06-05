> module Parser where
>
> import ParserBib
> import Control.Applicative
> import Control.Monad (guard)
> import Data.List (partition)

Implement the following parser for the alphabet {a,b}
 and {a,b,c}, resp.

ab: accepts all words that has as many 'a's as it has 'b's
(e.g.: "aaaabbbbaabb")

> abCheck :: Parser Int
> abCheck = (length . fst) <$> check pred abParser
>  where
>   pred (as,bs) = length as == length bs
>   abParser =
>    fmap (\ xs -> partition (== 'a') xs)
>         (many (char_ 'a' <|> char_ 'b'))

> char_ :: Char -> Parser Char
> char_ c = char c >> yield c
>
> abCheck' :: Parser ()
> abCheck' = const () <$> check pred abParser
>  where
>   pred (noA,noB) = noA == noB
>   abParser :: Parser (Int,Int)
>   abParser =
>        (\(noA,noB) -> (noA + 1, noB)) <$> (char 'a' *> abParser)
>    <|> (\(noA,noB) -> (noA, noB + 1)) <$> (char 'b' *> abParser)
>    <|> yield (0,0)

> abCheck'' :: Parser ()
> abCheck'' = const () <$> check pred abParser
>  where
>   pred diff = diff == 0
>   abParser :: Parser Int
>   abParser =
>        (+1) <$> (char 'a' *> abParser)
>    <|> (\x -> x - 1) <$> (char 'b' *> abParser)
>    <|> yield 0

Without `check`, we can just use monadic parser combinators!

> ab :: Parser ()
> ab = do
>   x <- abParser
>   guard (x == 0)
>  where
>   abParser =
>        (+1) <$> (char 'a' *> abParser)
>    <|> (\x -> x - 1) <$> (char 'b' *> abParser)
>    <|> yield 0

abc: accepts all words that have the same number of
'a's, 'b's and 'c's (e.g..: "abcabcabccbaaaacbcbcb")

> abcCheck'' :: Parser ()
> abcCheck'' = const () <$> check pred abcParser
>  where
>   pred diffPair = diffPair == (0,0)
>   abcParser :: Parser (Int,Int)
>   abcParser =
>        (\(abDiff,bcDiff) -> (abDiff + 1, bcDiff)) <$>
>           (char 'a' *> abcParser)
>    <|> (\(abDiff,bcDiff) -> (abDiff - 1, bcDiff + 1)) <$>
>           (char 'b' *> abcParser)
>    <|> (\(abDiff,bcDiff) -> (abDiff, bcDiff - 1)) <$>
>            (char 'c' *> abcParser)
>    <|> yield (0,0)

Implement two versions for each parser: one naive one using `check` and one implementation without using `check`.
