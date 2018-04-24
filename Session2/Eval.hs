
> data Expr a = Expr a :+: Expr a
>             | Expr a :/: Expr a
>             | Num Float
>             | Var a

> type Env a b = a -> Maybe b
> 
> add :: Eq a => a -> b -> Env a b -> Env a b
> add k v env = \k' -> if k == k' then Just v else env k'
> 
> data ExprEnv a = EE (Env a Float)

> --   x + 4              | x |-> 5
> -- = 9
> expr1 :: Expr Int
> expr1 = Var 1 :+: Num 4

> evalWithEnv :: ExprEnv a -> Expr a -> Maybe Float

