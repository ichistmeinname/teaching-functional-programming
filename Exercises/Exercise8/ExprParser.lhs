> import Parser

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

> instance Read Expr where
>     readsPrec _ = undefined
