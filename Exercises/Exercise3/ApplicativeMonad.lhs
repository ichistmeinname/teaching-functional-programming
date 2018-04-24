Prove that if you have a valid monad instance `m` with appropriate `(>>=)` and `return` implementations that the definitions

> (<*>) :: Monad m => m (a -> b) -> m a -> m b
> (<*>) ff fx = ff >>= \f -> fx >>= \x -> return (f x)
>
> pure :: Monad m => a -> m a
> pure = return

form a valid applicative instance as well, i.e. prove the applicative laws by assuming the monad laws.
