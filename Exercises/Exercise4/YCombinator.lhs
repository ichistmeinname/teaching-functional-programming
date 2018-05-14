> import Prelude hiding (repeat, fix)

In class we defined the `Y`-combinator using a `newtype` and the function `fix :: (a -> a) -> a` definiert.
There is a more convenient way using recursion.

We define the fixpoint combinator in Haskell using recursion as follows.

    ~~~{.literateaskell}
    > fix :: (a -> a) -> a
    > fix f = f (fix f)
    ~~~

We will use a different implementation (explanation follows later).

> fix :: (a -> a) -> a
> fix f = x
>   where x = f x

Now we can define every recursive function using `fix` by using the odinary function definition with an additional functional argument as argument to `fix`, 

For example, we can define the `fib` as follows.

> fib :: Int -> Int
> fib = fix (\f n -> if n < 2 then n else f (n-1) + f (n-2))

    ghci> map fib [0..9]  
    [0,1,1,2,3,5,8,13,21,34]


We can also define functions that have more than one argument with the same approach.

> append :: [a] -> [a] -> [a]
> append = fix f
>   where f _   []     ys = ys
>         f app (x:xs) ys = x : app xs ys

Hopefully, it does not come as a surprise that `append` behaves the same as `(++)`.

    ghci> append [1,2,3] [4,5]  
    [1,2,3,4,5]

Next up, we define `repeat` by using `fix`.

> repeat :: a -> [a]
> repeat = fix (\rep x -> x : rep x)

An alternative version (eta-reduced) looks as follows.

> repeat' :: a -> [a]
> repeat' x = fix (x:)

This definition corresponds to `fix (\xs -> x:xs)`, that is, the first argument passed to `fix` is not a function, but a list!
Such a call corresponds to the recursive definition of an infinite list containing only `x`s.

    ~~~{.literatehaskell}
    > xs = x : xs
    ~~~

Hence, we construct a cyclic structure.
Now the interesting thing to observe here is that the definition of `fix` that we used above produces this (efficient!) version of an infinite list containing only `x`, because we only need constant space.
If we'd defined `fix` recursive insteaf of cyclic, the above construction would yield a definition equivalent to the following definition.

> repeat_ x = x : repeat_ x

The recursives structure needs as much space as elements are constructed.

Since we're already talking about efficiency: how does an efficient definition of `fib` look like? Of course, we use an accumulator!

> fib'acc :: Int -> Int
> fib'acc = fix (\f x y n -> if n==0 then x else f y (x+y) (n-1)) 0 1

    ghci> map fib'acc [0..9]  
    [0,1,1,2,3,5,8,13,21,34]


Alternatively, we compute an infinite list of fibonacci numbers and select the `n`th element.

> fib'list :: Int -> Int
> fib'list n = fix (\fibs -> 0:1:zipWith (+) fibs (tail fibs)) !! n

This definition corresponds to the following code.

> fib' n = fibs !! n
>  where fibs = 0:1:zipWith (+) fibs (tail fibs)

    ghci> map fib'list [0..9]  
    [0,1,1,2,3,5,8,13,21,34]

This function also uses `fix` to compute an list. However, this time the resulting list is not cyclic, because `zipWith` builds new constructors. This observation is not a potential drawback, because the list of all fibonacci numbers cannot be cyclic, since it contains an infinite number of different values.

**Remark:** In class we defined `fix` as follows.

> newtype Fix a = Fix { app :: Fix a -> a }
>
> fix' = \f -> (\x -> f (app x x)) (Fix (\x -> f (app x x)))
