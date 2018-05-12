Based on the solution of exercise no. 2, we want to read `Let`-statements for the arithmetic expressions as well as the environment to evaluate variables.
Again, we want to implement the solution with CPS.

> data Expr a = Expr a :+: Expr a
>             | Expr a :/: Expr a
>             | Num Int
>             | Var a
>             | Let a Int (Expr a)

> type Env a b = a -> Maybe Int

> evalCPS :: Eq a => Env a b -> Expr a -> (Int -> Maybe Int) -> Maybe Int
> evalCPS env (Num x) k = k x
> evalCPS env (e1 :+: e2) k = 
>         evalCPS env e1 (\v1 -> evalCPS env e2 (\v2 -> k (v1 + v2)))
> evalCPS env (e1 :/: e2) k =
>         evalCPS env e2 (\v2 ->
>           if v2 == 0 then Nothing
>           else evalCPS env e1 (\v1 -> k (div v1 v2))) 
> evalCPS env (Var v) k = 
>           case env v of
>              Nothing -> Nothing
>              Just y  -> k y  
> evalCPS env (Let v x exp) k = 
>         evalCPS (\z -> if v == z then Just x else env z) exp k