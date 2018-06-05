> import Parser
> import Data.Char (ord)
> import Control.Monad (guard)
> import Control.Applicative (Alternative(..))

Arithmetic expressions again!

Implement a parser for a data type for arithmetic expressions that you learnt to love over the last few weeks. Use the following grammar to define the parser using the combinators presented in class.

    Expr   ::= Term Expr1
    Expr1  ::= PlusOp Term Expr1 | Ɛ
    Term   ::= Factor Term1
    Term1  ::= MultOp Factor Term1 | Ɛ
    Factor ::= '(' Expr ')' | Num
    PlusOp ::= '+' | '-'
    MultOp ::= '*' | '`div`' | '`mod`'
    Num    ::= '(' '-' Nat ')' | Nat
    Nat    ::= Digit Nat | Digit
    Digit  ::= 0 | ... | 9


> data Expr = Number Int
>           | Binary Bin Expr Expr

> data Bin = Plus | Minus | Times | Div | Mod

For testing purposes, we also define a `Show` instance for `Expr.

> instance Show Bin where
> 
>     show Plus = ":+:"
>     show Minus = ":-:"
>     show Times = ":*:"
>     show Div   = ":/:"
>     show Mod   = ":%:"

> instance Show Expr where
> 
>     show (Number n) = "Number " ++ show n
>     show (Binary bin l r) =    "(" ++ show l ++ ") "
>                             ++ show bin
>                             ++ " (" ++ show r ++ ")"

Implement an instance of Read for the data type Expr using your parser.

We start with the parser for (natural) numbers.
First of, a natural number is a sequence of digits.

> pNat :: Parser [Int]
> pNat = some pDigit

Now we only need to parse individual digits.
We implement the corresponding parser in two steps.
First, we use the choice operator `<|>` to choose between
the possible digits from '0' to '9'.
Second, we translate the digit character into a "real" digit using `toNumeral`.

> pDigit :: Parser Int
> pDigit = toNumeral <$> foldr1 (<|>) ds where
>     ds = map (\c -> char c *> yield c) ['0' .. '9']
>     toNumeral c = ord c - ord '0'

Now we can define the parser that corresponds to `Num` in our grammar.
A number is either a negative or positive and in the end, the list of
digits is interpreted as an integer value using `numeric`.

> -- Num    ::= '(' '-' Nat ')' | Nat
> pNumber :: Parser Int
> pNumber = pNegNat <|> pNat'
>     where pNegNat = char '(' *> char '-' *> pNat' <* char ')'
>           pNat'   = numeric <$> pNat
>
> numeric :: [Int] -> Int
> numeric = foldl ((+) . (10 *)) 0

Using a helper function `operator` to parse specific operator keywords,
we define parser corresponding to `MultOp` and `PlusOp`.

> operator :: String -> a -> Parser a
> operator str x = word str *> yield x
>
> -- MultOp ::= '*' | '`div`' | '`mod`'
> pMultOp :: Parser Bin
> pMultOp =     operator "*" Times
>           <|> operator "`div`" Div
>           <|> operator "`mod`" Mod
> 
> -- PlusOp ::= '+' | '-'
> pPlusOp :: Parser Bin
> pPlusOp =     operator "+" Plus
>           <|> operator "-" Minus

Now we can define the parser for the important rules of the grammar.
The easiest rule is for 'Factor'.

> -- Factor ::= '(' Expr ')' | Num
> pFactor :: Parser Expr
> pFactor = char '(' *> pExpr <* char ')' <|> Number <$> pNumber

An expression is then defined by parsing an `Expr1` and apply a `Term` parser on the result.

> -- Expr   ::= Term Expr1
> pExpr :: Parser Expr
> pExpr = pTerm >>= pExpr1

The same application pattern applies to `Term`, `Expr1` and `Term1`.

> -- Term   ::= Factor Term1
> pTerm :: Parser Expr
> pTerm = pFactor >>= pTerm1
 
> -- Expr1  ::= PlusOp Term Expr1 | Ɛ
> pExpr1 :: Expr -> Parser Expr
> pExpr1 a = (binOp a <$> pPlusOp <*> (pTerm >>= pExpr1)) <|> yield a

> -- Term1  ::= MultOp Factor Term1 | Ɛ
> pTerm1 :: Expr -> Parser Expr
> pTerm1 a = (binOp a <$> pMultOp <*> (pFactor >>= pTerm1)) <|> yield a

> binOp :: Expr -> Bin -> Expr -> Expr
> binOp = flip Binary

The overall Read instance can then be implemented by unwraping the `pExpr` parser.

> instance Read Expr where
>     readsPrec _ = parser pExpr

Of course, it is reasonable to test our implementation.

   λ> read "3+4*(5`div`3)" :: Expr
   (Number 3) :+: ((Number 4) :*: ((Number 5) :/: (Number 3)))

While this example works like a charm, the world does not look as pretty for a second one.

   λ> read "3 + 4*(5`div`3)" :: Expr
   *** Exception: Prelude.read: no parse

This behavior is due to the fact that we did not consider any whitespaces.
A quick fix is to filter all whitespaces in the string to read.

> newtype WhitespaceExpr = WSE Expr

> instance Show WhitespaceExpr where
>     show (WSE e) = show e

> instance Read WhitespaceExpr where
>     readsPrec _ = fmap (\(e,str) -> (WSE e,str)) . parser pExpr . filter (/= ' ')

    λ> read "3 + 4*(5`div`3)" :: WhitespaceExpr
    (Number 3) :+: ((Number 4) :*: ((Number 5) :/: (Number 3)))

Of course, the best way is to parse the whitespaces at the right places in the above parser definitions.
