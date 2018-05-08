Implement an evaluation function for arithmetic expressions based on the code in class (using values and basic operations only) in continuations passing style.
Give a direct encoding, that is, without using the continuation monad.

> data Expr = Num Int
>           | Expr :+: Expr
>           | Expr :/: Expr