> {-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
    
Let's consider the following monadic type class to trace information during the execution of a program.

> class Monad m => MonadWriter w m where
>    tell :: Monoid w => w -> m ()
>
> -- class Monoid mi where
> --   mappend :: mi -> mi -> mi
> --   mempty  :: mi

We specify the tracing information `w` to be a `monoid` in order to collect all tracing information via `mappend`.

1. Implement a  type `Writer w a` as an instance of `MonadWriter` as well as the function

> data Writer w a = Writer { runWriter :: (a,w) }
>   deriving Show
 
    that executes a computation accordingly and yields the result of the computation as well as all tracing information.

> instance Monoid w => Functor (Writer w) where
>    fmap f w = w >>= return . f

> instance Monoid w => Applicative (Writer w) where
>     pure = return
>     (<*>) = undefined

> instance Monoid w => Monad (Writer w) where
>     -- return :: a -> Writer w a
>     return x = Writer (x, mempty)
>     -- (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
>     Writer (x,w) >>= f =  -- f x -- does not obey monad laws
>        let Writer (y,v) = f x
>        in Writer (y,w `mappend` v)
>
> instance Monoid w => MonadWriter w (Writer w) where
>     -- tell :: w -> Writer w ()
>     tell tr = Writer ((), tr)

2. In order to see this type class in action, define a function

> hanoi :: MonadWriter String m => Int -> m ()
> hanoi = undefined

    that traces the moves you need to make in order to solve the puzzle of hanoi (according to the rules [towers of hanoi][hanoi]).

[hanoi]: http://de.wikipedia.org/wiki/Türme_von_Hanoi
