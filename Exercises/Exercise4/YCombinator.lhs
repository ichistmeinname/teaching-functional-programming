> import Prelude hiding (repeat, fix)
> import Interpreter

In class we defined the `Y`-combinator using a `newtype` and the function `fix :: (a -> a) -> a` definiert.
There is a more convenient way using recursion.

1. Give a definition of the `fix` using recursion.

> fix :: (a -> a) -> a
> fix f = f (fix f)

2. Define the functions `fib`, `append` and `repeat` by means of `fix`.

> fib = fix (\f -> \n -> if n < 2 then n else f (n-1) + f (n-2))
> append = fix (\f -> \xs ys -> case xs of
>                                []      -> ys
>                                (z:zs)  -> z : f zs ys)
> repeat = fix (\f -> \x -> x : f x)



3. Can you also give the efficient (accumulator) version for `fib` using `fix`.

> fibAcc = fix (\f -> \n np np1 -> if n == 0 then np else f (n-1) np1 (np + np1))

**Remark:** In class we defined `fix` as follows.

> newtype Fix a = Fix { app :: Fix a -> a }
>
> fix' = \f -> (\x -> f (app x x)) (Fix (\x -> f (app x x)))
