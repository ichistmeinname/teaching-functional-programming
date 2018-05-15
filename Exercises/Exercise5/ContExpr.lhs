Implement an evaluation function for arithmetic expressions based on the code in class (using values and basic operations only) in continuations passing style.
Give a direct encoding, that is, without using the continuation monad.

> data Expr = Num Int
>           | Expr :+: Expr
>           | Expr :/: Expr

In order to define an expression evaluator of type `Expr -> Maybe Int` in CPS, we need a helper function that takes a continuation of type `Int -> Maybe Int` as function.

> evaluate :: Expr -> (Int -> Maybe Int) -> Maybe Int

The base case for a numeric value is pretty straightforward, as we only need to apply the continuation to the integer value `n`.
Here, we can already observe that we need to pass `Just` as inital continuation in the definition of the top-level function `evalExpr`.

> evaluate (Num n) c = c n

For the binary operator we need to define the continuation we want to apply next explicitly.
In case of `:+:`, we evaluate the left argument first, evaluate the right argument withing that continuation and in the second contiunation we then combine both numeric values using addition.

> evaluate (e1 :+: e2) c =
>   evaluate e1 (\v1 ->
>     evaluate e2 (\v2 -> c (v1 + v2)))

In case of `:/:` we apply the same basic idea, however, we need to handle the additional "bad case" of division by 0.

> evaluate (e1 :/: e2) c =
>   evaluate e2 (\v2 ->
>     if v2 == 0 then Nothing
>     else evaluate e1 (\v1 -> c (v1 `div` v2)))

In the end we define the top-level function `evalExpr` by using `Just` as the starting continuation.

> evalExpr :: Expr -> Maybe Int
> evalExpr expr = evaluate expr Just

For testing purposes we try to evaluate the expressions we used in old exercises.
Fortunately, the result is as expected.

> testExpr1 :: Expr
> testExpr1 = Num 42 :+: (Num 1 :/: Num 0)

> testExpr2 :: Expr
> testExpr2 = Num 42 :+: (Num 1 :/: Num 1)

> testEval :: IO ()
> testEval = mapM_ go [(testExpr1,"ex1"), (testExpr2,"ex2")]
>  where
>    go (testExpr,name) =
>      putStrLn (name ++ ": " ++ show (evalExpr testExpr))

*Main> testEval
ex1: Nothing
ex2: Just 43