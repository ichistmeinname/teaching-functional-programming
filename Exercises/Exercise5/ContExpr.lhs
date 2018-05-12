Implement an evaluation function for arithmetic expressions based on the code in class (using values and basic operations only) in continuations passing style.
Give a direct encoding, that is, without using the continuation monad.

> data Expr = Num Int
>           | Expr :+: Expr
>           | Expr :/: Expr

> evalCPS :: Expr -> (Int -> Maybe Int) -> Maybe Int
> evalCPS (Num x) k = k x
> evalCPS (e1 :+: e2) k = 
>         evalCPS e1 (\v1 -> evalCPS e2 (\v2 -> k (v1 + v2)))
> evalCPS (e1 :/: e2) k =
>         evalCPS e2 (\v2 ->
>           if v2 == 0 then Nothing
>           else evalCPS e1 (\v1 -> k (div v1 v2)))

