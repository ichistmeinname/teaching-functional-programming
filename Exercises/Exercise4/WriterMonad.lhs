> {-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

Let's consider the following monadic type class to trace information during the execution of a program.

> class Monad m => MonadWriter w m where
>    tell :: Monoid w => w -> m ()

We specify the tracing information `w` to be a `monoid` in order to collect all tracing information via `mappend`.

1. Implement a  type `Writer w a` as an instance of `MonadWriter` as well as the function

> data Writer w a = Writer { runWriter :: (a,w) }

    that executes a computation accordingly and yields the result of the computation as well as all tracing information.

For convenience we introduce a `<>` as operator for `mappend`.

> (<>) :: Monoid m => m -> m -> m
> (<>) = mappend

The monad instance is similar to the state monad instance we defined in class, except we can only modify the state by accumilating all changes -- we cannot read the actions we've done before!

> instance Monoid w => Monad (Writer w) where
>   return x = Writer (x,mempty)
>   fx >>= f  = Writer (let (x,v) = runWriter fx
>                           (y,w) = runWriter (f x)
>                       in (y,v <> w))

> instance Monoid w => Applicative (Writer w) where
>   pure x     = Writer (x,mempty)
>   ff <*> fx  = Writer (let (f,v) = runWriter ff
>                            (x,w) = runWriter fx
>                        in (f x, v <> w))

> instance Monoid w => Functor (Writer w) where
>   fmap f (Writer (x,w)) = Writer (f x, w)

We define the following laws for writer monads.

  tell mempty    =  return ()
  tell (a <> b)  =  tell a >> tell b

These laws express that `tell` is a monoid-homomorphism from `w` to `m ()`.

Now we can define `tell` as follows and show that it is indeed a homomorphism as speficied.

> instance Monoid w => MonadWriter w (Writer w) where
>   tell w = Writer ((),w)

     tell mempty
  =  Writer ((),mempty)
  =  return ()
  
     tell (a <> b)
  =  Writer ((), a <> b)
  =  Writer (let (x,v) = ((),a)
                 (y,w) = ((),b)
              in (y,v <> w))
  =  Writer (let (x,v) = runWriter (Writer ((),a))
                 (y,w) = runWriter (Writer ((),b))
              in (y,v <> w))
  =  Writer (let (x,v) = runWriter (tell a)
                 (y,w) = runWriter (tell b)
              in (y,v <> w))
  =  Writer (let (x,v) = runWriter (tell a)
                 (y,w) = runWriter ((\_ -> tell b) x)
              in (y,v <> w))
  =  tell a >>= \_ -> tell b
  =  tell a >> tell b
 
2. In order to see this type class in action, define a function

> hanoi :: MonadWriter String m => Int -> m ()
> hanoi n = move n A B C

    that traces the moves you need to make in order to solve the puzzle of hanoi (according to the rules [Türme von Hanoi][hanoi]).

Since `String`s are monoids it's easiest to use them to trace the moves we make during a game of hanoi.

The game of hanoi has three potential positions to move a piece.

> data Tower = A | B | C
>  deriving Show

We define the `move` function recursively, where `n` is the number of plates that are used during the game.

> move :: MonadWriter String m => Int -> Tower -> Tower -> Tower -> m ()
> move 1 from _ to = tell $ "move disk from " ++ show from ++ " to " ++ show to ++ "\n"
> move n from over to =
>  do move (n-1) from to over
>     move 1 from over to
>     move (n-1) over from to

We test our implementation for a game with 5 plates.

> main = putStr (snd (runWriter (hanoi 5)))

  ghci> main
  move disk from A to C
  move disk from A to B
  move disk from C to B
  move disk from A to C
  move disk from B to A
  move disk from B to C
  move disk from A to C
  move disk from A to B
  move disk from C to B
  move disk from C to A
  move disk from B to A
  move disk from C to B
  move disk from A to C
  move disk from A to B
  move disk from C to B
  move disk from A to C
  move disk from B to A
  move disk from B to C
  move disk from A to C
  move disk from B to A
  move disk from C to B
  move disk from C to A
  move disk from B to A
  move disk from B to C
  move disk from A to C
  move disk from A to B
  move disk from C to B
  move disk from A to C
  move disk from B to A
  move disk from B to C
  move disk from A to C

At last, the attentive reader might observe that we do not actually need a monad for this specific problem. Instead we're just using the fact that `String` is a monoid.

> hanoi' :: Int -> String
> hanoi' n = move' n A B C
> 
> move' 1 from _ to =
>   "move disk from " ++ show from ++ " to " ++ show to ++ "\n"
> 
> move' n from over to =
>   move' (n-1) from to over ++
>   move' 1 from over to ++
>   move' (n-1) over from to

In the end, `hanoi'` computes the same `snd . runWriter . hanoi`.

[hanoi]: http://de.wikipedia.org/wiki/Türme_von_Hanoi
