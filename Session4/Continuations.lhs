Implement an evaluation function for arithmetic expressions based on the code in class (using values and basic operations only) in continuations passing style.
Give a direct encoding, that is, without using the continuation monad.

> data LExpr a = LNum Int
>              | LExpr a :.+.: LExpr a
>              | LExpr a :./.: LExpr a
>              | Var a
>              | Let a Int (LExpr a)
>
> type Env a b = a -> Maybe b
> type ExprEnv a = Env a Int
>
> emptyEnv = const Nothing

> evalWithLet :: Eq a => LExpr a -> ExprEnv a -> Maybe Int
> evalWithLet expr env = fmap fst (go expr c env)
>  where
>   c n = Just (n, env)
>   -- go :: LExpr a -> (ExprEnv a -> Maybe (Int, ExprEnv a))
>   -- go :: LExpr a -> (Int -> ExprEnv a -> Maybe (Int, ExprEnv a))
>   --    -> ExprEnv a -> Maybe (Int, ExprEnv a)
>   -- let x 
>   go (LNum v) c env = c v
>   go (e1 :.+.: e2) c env =
>       go e1 (\n1 ->
>         go e2 (\n2 -> c (n1+n2)) env) env
>   go (e1 :./.: e2) c env =
>       go e2 (\n2 ->
>         case n2 of
>           0 -> Nothing
>           _ -> go e1 (\n1 -> c (n1 `div` n2)) env) env
>   go (Var v) c env = env v >>= c
>                       -- case env v of
>                       -- Nothing -> Nothing
>                       -- Just v2 -> c v2
>   -- fmap :: (Int -> (Int,ExprEnv a))
>   --      -> Maybe Int -> Maybe (Int, ExprEnv a)
>   -- let x = v in e
>   go (Let x v e) c env =
>    go e c (\y -> if y == x then Just v else env y)
>
> data Expr = Num Int
>           | Expr :+: Expr
>           | Expr :/: Expr

> eval :: Expr -> Maybe Int
> eval expr = evalCPS expr c
>  where c = Just

> -- evalCPS :: Expr -> (Int -> Maybe r) -> Maybe r
> evalCPS :: Expr -> (Int -> Maybe Int) -> Maybe Int
> evalCPS (Num n) c = c n
> -- e1,e2 :: Expr
> evalCPS (e1 :+: e2) c =
>     evalCPS e1 (\n1 ->
>     evalCPS e2 (\n2 -> c (n1 + n2)))
> evalCPS (e1 :/: e2) c =
>     evalCPS e2 (\n2 ->
>      case n2 of
>        0 -> Nothing
>        _ -> evalCPS e1 (\n1 -> c (n1 `div` n2)))

> test0 = Num 5               -- Just 5
> test1 = Num 42 :+: Num 1    -- Just 43
> test2 = Num 42 :/: Num 1    -- Just 42
> test3 = Num 42 :/: Num 0    -- Nothing

> testL0 = LNum 5               -- Just 5
> testL1 = LNum 42 :.+.: LNum 1    -- Just 43
> testL2 = LNum 42 :./.: LNum 1    -- Just 42
> testL3 = LNum 42 :./.: LNum 0    -- Nothing
> testL4 = Var 'x'
> testL5 = Let 'x' 42 (Var 'x')
> testL6 = Let 'x' 42 (LNum 5 :.+.: Var 'x')
> testL7 = Let 'x' 42 (Var 'x' :.+.: Var 'x')
> testL8 = Let 'x' 42 (Var 'y')
> testL9 = Let 'x' 42 (Var 'x' :.+.: LNum 5)
