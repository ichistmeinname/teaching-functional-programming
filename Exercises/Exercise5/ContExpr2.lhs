Based on the solution of exercise no. 2, we want to readd `Let`-statements for the arithmetic expressions as well as the environment to evaluate variables.
Again, we want to implement the solution with CPS.

> data Expr a = Expr a :+: Expr a
>             | Expr a :/: Expr a
>             | Num Int
>             | Var a
>             | Let a Int (Expr a)