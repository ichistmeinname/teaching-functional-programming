> import Prelude hiding (repeat, fix)
> import Interpreter

In class we defined the `Y`-combinator using a `newtype` and the function `fix :: (a -> a) -> a` definiert.
There is a more convenient way using recursion.

1. Give a definition of the `fix` using recursion.

> fixR :: (a -> a) -> a
> fixR f = f (fixR f) 

2. Define the functions `fib`, `append` and `repeat` by means of `fix`.

> fib = undefined
> append = undefined
> repeat = undefined

3. Can you also give the efficient (accumulator) version for `fib` using `fix`.

**Remark:** In class we defined `fix` as follows.

> newtype Fix a = Fix { app :: Fix a -> a }
>
> fix' = \f -> (\x -> f (app x x)) (Fix (\x -> f (app x x)))
