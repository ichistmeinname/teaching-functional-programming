Based on the solution of exercise no. 2, we want to readd `Let`-statements for the arithmetic expressions as well as the environment to evaluate variables.
Again, we want to implement the solution with CPS.

> data Expr a = Expr a :+: Expr a
>             | Expr a :/: Expr a
>             | Num Int
>             | Var a
>             | Let a Int (Expr a)

We quickly recapitulate the necessary helper functions to handle environments.

> type Env a b = a -> Maybe b
> type ExprEnv a = Env a Int
>
> insert :: Eq a => a -> b -> Env a b -> Env a b
> insert k v env = \ k' -> if k == k' then Just v else env k'

In order to handle an potentially failure result and an environment, we used the following setup.

    evalExpr' :: Eq a => Expr a -> ExprEnv a -> Maybe (Int, ExprEnv a)

That is, we need to propagate the (potentially changed!) environment through all recursive evaluation calls and --- in case of a non-failing result --- project to the first component of the pair.

    evalExpr :: Eq a => Expr a -> ExprEnv a -> Maybe Int
    evalExpr expr env = fst (evalExpr' expr env)

Using the standard transformation idea using CPS, we need a helper function that adds an additional continuation argument.
Since we can interpret the last part of the above signature, i.e., `ExprEnv a -> Maybe (Int, ExprEnv a)` as the "state-based" result, we end up with the following helper function.

> evaluate :: Eq a => Expr a -> (Int -> ExprEnv a -> Maybe (Int,ExprEnv a))
>                  -> ExprEnv a -> Maybe (Int,ExprEnv a)

Observe that the continuation has two arguments.
The continuation has to make a computation based on the enviornment (in case of a `Let`-binding), thus, it does not suffice to apply the numeric value alone.
The implementation for numeric values and operations follows then pretty straightforward from their non-state-based counterparts.

> evaluate (Num n) c env = c n env
> evaluate (e1 :+: e2) c env =
>   evaluate e1 (\v1 env1 ->
>     evaluate e2 (\v2 env2 -> c (v1 + v2) env2) env1) env
> evaluate (e1 :/: e2) c env =
>   evaluate e2 (\v2 env2 ->
>     if v2 == 0 then Nothing
>     else evaluate e1 (\v1 env1 -> c (v1 `div` v2) env1) env2) env

In case of a variable we need to make a case distinction based on the lookup in the enviornment and continute in case of a successful lookup.
Otherwise we end the evaluation by yielding `Nothing`.

> evaluate (Var i) c env = case env i of
>                            Nothing -> Nothing
>                            Just v  -> c v env

In case of a `Let`-binding, we evaluate the expression argument in the changed environment that is simply used as argument for the recursivle call of `evaluate` (the `Var`-rule is the only case that breaks this rule).

> evaluate (Let x v expr) c env =
>   evaluate expr (\v1 env1 -> c v1 env1) (insert x v env) 


> evaluate' (Num n) c = c n
> evaluate' (e1 :+: e2) c =
>   evaluate' e1 (\v1 ->
>     evaluate' e2 (\v2 -> c (v1 + v2)))
> evaluate' (e1 :/: e2) c =
>   evaluate' e2 (\v2 ->
>     if v2 == 0 then const Nothing
>     else evaluate' e1 (\v1 -> c (v1 `div` v2)))
> evaluate' (Var i) c = \env -> case env i of
>                                 Nothing -> Nothing
>                                 Just v  -> c v env
> evaluate' (Let x v expr) c =
>   evaluate' expr (\v1 -> c v1) . insert x v

The original three rules do not differ (except for handling division by 0 that needs to yield a function `const Nothing`) from the solution for the first continuation assignment.
The state is not visible in the rules that do not make use of it.

In the end, we define the top-level function as follows.
Note that the inital continuation resembeles `Just . return`, where `return` is instantiated as the state-monad.
That is, even in this more complicated implementation, the definition mirrors that we simply stack two monads: state and maybe.

> evalExpr :: Eq a => Expr a -> ExprEnv a -> Maybe Int
> evalExpr expr env = fmap fst (evaluate' expr (\n env1 -> Just (n, env1)) env)

> testExpr1 :: Expr Char
> testExpr1 = Num 42 :+: (Num 1 :/: Num 0)

> testExpr2 :: Expr Char
> testExpr2 = Num 42 :+: (Num 1 :/: Num 1)

> testExpr3 :: Expr Char
> testExpr3 = Let 'x' 1 (Num 42 :+: (Num 1 :/: Var 'x'))

> testExpr4 :: Expr Char
> testExpr4 = Let 'x' 0 (Num 42 :+: (Num 1 :/: Var 'x'))

> testExpr5 :: Expr Char
> testExpr5 = Let 'x' 0 (Var 'x') :+: Let 'x' 1 (Num 1 :/: Var 'x')

> emptyEnv :: ExprEnv a
> emptyEnv _ = Nothing

> testEval :: IO ()
> testEval = mapM_ go [(testExpr1,"ex1"), (testExpr2,"ex2")
>                     ,(testExpr3,"ex3"), (testExpr4,"ex4")
>                     ,(testExpr5,"ex5")]
>  where
>    go (testExpr,name) =
>      putStrLn (name ++ ": " ++ show (evalExpr testExpr emptyEnv))