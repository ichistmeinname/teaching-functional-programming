> {-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

Let's consider the following monadic type class to trace information during the execution of a program.

> class MonadWriter w m where
>    tell :: Monoid w => w -> m ()

We specify the tracing information `w` to be a `monoid` in order to collect all tracing information via `mappend`.

1. Implement a  type `Writer w a` as an instance of `MonadWriter` as well as the function

> data Writer w a {- <fill in your definition here> -}
>
> runWriter :: Monoid w => Writer w a -> (a,w)
> runWriter = undefined

    that executes a computation accordingly and yields the result of the computation as well as all tracing information.

2. In order to see this type class in action, define a function

> hanoi :: MonadWriter String m => Int -> m ()
> hanoi = undefined

    that traces the moves you need to make in order to solve the puzzle of hanoi (according to the rules [Türme von Hanoi][hanoi]).

[hanoi]: http://de.wikipedia.org/wiki/Türme_von_Hanoi

**Remark**: As already mentioned during the lecture, we need some language extensions, i.e. `{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}, to use a multi-parameter type class as above.
