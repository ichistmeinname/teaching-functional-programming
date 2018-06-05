> import Prelude hiding      ( product )

Let's define the following data types

> data Composed f g a = Comp { composed :: f (g a) }
> data Product  f g a = Prod { product  :: (f a, g a)}

to represent the composition and multiplication of type constructors.

(1) Before we implement the `Applicative` instances, we define the `Functor` instances first.

In case of `Composed`, we just propagate the function `f` to the underlying functors.

> instance (Functor f, Functor g) => Functor (Composed f g) where
>     fmap f = Comp . fmap (fmap f) . composed

In case of `Product` the implementation is even simpler, we just use `fmap` on both components.

> instance (Functor f, Functor g) => Functor (Product f g) where
>     fmap f (Prod (x, y)) = Prod (fmap f x, fmap f y)

The applicative instances can be defined accordingly.

For `Product`, the two functions of the left arguments are applied to the values of the right argument.

> instance (Applicative f, Applicative g) => Applicative (Product f g) where
>     pure x = Prod (pure x, pure x)
>     Prod (fx, fy) <*> Prod (x, y) = Prod (fx <*> x, fy <*> y)

This time, the implementation for `Composed` is the simpler one; at least for `pure`,
where we just use the underlying applicative instances in the correct order.
Thus, embedding one applicative effect in the other.

> instance (Applicative f, Applicative g) => Applicative (Composed f g) where
>     pure = Comp . pure . pure
>     Comp fun <*> Comp x = Comp (pure (<*>) <*> fun <*> x)

Looking more closely at `Comp fun <*> Comp x`, we see that we have
`fun :: f (g (a -> b))` and `x :: f (g a)`.
If we could access the value of type `g (a -> b)`, then we only need to propagate it via `(<*>)`.
That is, we have `op f <*> x` for an appropriate `op`.
Now, we cann pass the application of `(<*>)` as function using the expression `op = pure (<*>) <*> f`.
In the end, we get the implementation above.

Note that the generic scheme `pure f <*> x <*> y` is known as `liftA2`.

All applicative laws for `Product` are simple to prove, since we only need to
apply the corresponding laws of the underlying components.

In case of `Composed`, the proof are a bit more complicated.

      fmap id
    = {- Definition of fmap -}
      Comp . fmap (fmap id) . composed
    = {- fmap id = id, with g Functor -}
      Comp . fmap id . composed
      {- fmap id = id, with f Functor -}
    = Comp . id . composed
    = Comp . composed
    = id
    
      fmap (f . g)
    = Comp . fmap (fmap (f . g)) . composed
    = {- fmap homomorphism (inner) -}
      Comp . fmap (fmap f . fmap g) . composed
    = {- fmap homomorphism (outer) -}
      Comp . fmap (fmap f) . fmap (fmap g) . composed
    = fmap f . Comp . fmap (fmap g) . composed)
    = fmap f . fmap g . Comp. composed
    = fmap f . fmap g
    
So, now we have shown that `Composed` fulfills the functor laws.

      pure f <*> Comp x
    = {- Definition pure, (<*>) -}
      Comp (pure (<*>) <*> (pure (pure f)) <*> x)
    = {- pure homomorph für f-}
      Comp (pure((<*>) pure f) <*> x)
    = {- Funktor/Applicative für g -}
      Comp (fmap ( (<*>) pure f) x)
    = {- partielle Applikation -}
      Comp (fmap (pure f <*>) x)
    = {- (pure f <*>) = \y -> pure f <*> y = \y -> fmap f y = fmap f -}
      Comp (fmap (fmap f) x)
    = {- Definition fmap -}
      fmap f (Comp x)

Now we now that `pure` and `fmap` behave as expected.

For the applicative laws

    -- Composition:
    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    
    -- pure homomorphism:
    pure f <*> pure x = pure (f x)
    
    -- exchance rule:
    u <*> pure x = pure ($ x) <*> u

we reason as follows.

    -- pure homomorphism:
    
      pure f <*> pure x
    = {- Functor/Applicative-rule for Composed -}
      fmap f (pure x)
    = {- Def. pure -}
      fmap f (Comp (pure (pure x)))
    = {- Def. fmap -}
      Comp (fmap (fmap f) (pure (pure x)))
    = {- Functor/Applicative for f -}
      Comp (pure (fmap f) <*> pure (pure x))
    = {- pure homomorph -}
      Comp (pure (fmap f (pure x)))
    = {- Functor/Applicative for g -}
      Comp (pure (pure (f x)))
    = {- Def. pure -}
      pure (f x)
    
    -- exchange rule:
    
      Comp f <*> pure x
    = {- Def. pure -}
      Comp f <*> Comp (pure (pure x))
    = {- Def. (<*>) -}
      Comp (pure (<*>) <*> f <*> pure (pure x))
    = {- exchange rule for f -}
      Comp (pure ($ (pure x)) <*> (pure (<*>) <*> f))
    = {- composition for f -}
      Comp (pure (.) <*> pure ($ (pure x)) <*> pure (<*>) <*> f)
    = {- pure/f homomorphism -}
      Comp (pure ((.) ($ (pure x))) <*> pure (<*>) <*> f)
    = {- pure/f homomorphism -}
      Comp (pure ((.) ($ (pure x)) (<*>)) <*> f)
    = {- reasoning on a side-note (*) -}
      Comp (pure ((<*>) (pure ($ x))) <*> f)
    = {- pure/f homomorphism -}
      Comp (pure (<*>) <*> pure (pure ($ x)) <*> f)
    = {- Def. (<*>) -}
      Comp (pure (pure ($ x))) <*> Comp f
    = {- Def. pure -}
      pure ($ x) <*> Comp f
    
    More details for (*):
    
      (.) ($ (pure x)) (<*>)
    = {- Infix auf (.) -}
      ($ (pure x)) . (<*>)
    = {- eta-expansion -}
      \y -> ($ (pure x)) ((<*>) y)
    = {- ($ x) f = f x -}
      \y -> (<*>) y (pure x)
    = {- infix for (<*>) -}
      \y -> y <*> pure x
    = {- exchange rule for g -}
      \y -> pure ($ x) <*> y
    = {- prefix for (<*>) -}
      \y -> (<*>) (pure ($ x)) y
    = {- eta-reduction -}
      (<*>) (pure ($ x))

(2) Is it possible to define a instance of Monad for the data type Composed? If so, give an implementation, otherwise explain why this is not possible. How about Product?

It is not possible to define a monad instance for `Composed.
We can observe that function `fun` passed to `(>>=)` is of type
`a -> Composed f g b`.
Consider the `Composed fgx`.
First, we need to access the inner value of type `a`.
We can do this using `>>=`, that is, `fgx >>= \gx -> gx >>= \x -> ...`.
However, when we apply the function after these two `>>=`s, we yield the wrong type.
Since there is no other way to access the value of type `a`, there is also no way
to give a reasonable way to define `>>=`.


In contrast, a monad instance for `Product` is indeed possible.
Let us define the following helper functions to access one component of `Product`.

> pfst :: Product f g a -> f a
> pfst = fst . product
>
> psnd :: Product f g a -> g a
> psnd = snd . product

It holds the following equality.

    Prod (pfst p, psnd p) = p
    
Now, we can transfer the behavior to the corresponding components.

> instance (Monad f, Monad g) => Monad (Product f g) where
>    return x = Prod (return x, return x)
>    Prod (x, y) >>= f = Prod (x >>= pfst . f, y >>= psnd . f)

On top of that, we can observe that the following equations hold.

    pfst (Prod (x, y) >>= f) = pfst (Prod (x >>= pfst . f, y >>= psnd . f))
                             = x >>= pfst . f
    -- or shorter
    pfst (p >>= f) = pfst p >>= pfst . f

The analogue equation holds for `psnd` as well.
