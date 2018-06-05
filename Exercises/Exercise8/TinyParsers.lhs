> import Parser
> import Control.Monad (guard)
> import Control.Applicative (Alternative(..))

Implement the following parser for the alphabet {a,b} and {a,b,c}, resp.

* ab: accepts all words that has as many as as it has bs (e.g.: "aaaabbbbaabb")
* abc: the same as ab but with an additional condition cs (e.g..: "abcabcabccbaaaacbcbcb")

  Implement two version for each parser: one naive one using check and one implementation without using check.

> abCheck :: Parser ()
> abCheck = const () <$> check (\(a,b) -> a == b) ab1'
>  where
>   ab1' = (\_ (a,b) -> (a+1, b)) <$> char 'a' <*> ab1'
>      <|> (\_ (a,b) -> (a, b+1)) <$> char 'b' <*> ab1'
>      <|> yield (0,0)

A version using `check` counts all occurrences of 'a's and 'b's and checks if they agree.
If so, we yield `()` otherwise the parser fails on run-time with a parse error.

The same idea can be applied using monadic parser combinators.
Instead of using `check` we use `>>=` to take a look at the current status of 'a's and 'b's and only succeed if both numbers agree.
 
> ab :: Parser ()
> ab = ab1' >>= \(a,b) -> guard (a == b)
>  where
>   ab1' = (\_ (a,b) -> (a+1, b)) <$> char 'a' <*> ab1'
>      <|> (\_ (a,b) -> (a, b+1)) <$> char 'b' <*> ab1'
>      <|> yield (0,0)

An alternative version without monadic parser combinators is a bit more complicated.
Instead of counting all occurrences we use two local parsers `a` and `b` that indicate that we need at least one more 'a' or 'b' to have a valid parse.

> ab2 :: Parser ()
> ab2 = (char 'a' *> b *> ab2)
>   <|> (char 'b' *> a *> ab2)
>   <|> yield ()
>  where
>   a = char 'b' *> a *> a <|> char 'a'
>   b = char 'a' *> b *> b <|> char 'b' 

The first two implementations of parsers above can be adapted to work for the second parser as well.

> abcCheck :: Parser ()
> abcCheck = const () <$> check (\(a,b,c) -> a == b && b == c) abc1'
>  where
>   abc1' = (\_ (a,b,c) -> (a+1,b,c)) <$> char 'a' <*> abc1'
>       <|> (\_ (a,b,c) -> (a,b+1,c)) <$> char 'b' <*> abc1'
>       <|> (\_ (a,b,c) -> (a,b,c+1)) <$> char 'c' <*> abc1'
>       <|> yield (0,0,0)

> abc :: Parser ()
> abc = abc1' >>= \(a,b,c) -> guard (a == b && b == c)
>  where
>   abc1' = (\_ (a,b,c) -> (a+1,b,c)) <$> char 'a' <*> abc1'
>       <|> (\_ (a,b,c) -> (a,b+1,c)) <$> char 'b' <*> abc1'
>       <|> (\_ (a,b,c) -> (a,b,c+1)) <$> char 'c' <*> abc1'
>       <|> yield (0,0,0)

The idea used in `ab2` can, however, not be adapted to this parser. Feel free to try, but in contrast to `ab2`, the decision which parser to use next depends on the current context.
A context-sensitive parser is always a sign for a usage of monadic parser combinators!
