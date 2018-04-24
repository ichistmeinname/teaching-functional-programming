> {-# LANGUAGE EmptyCase #-}

> import Prelude hiding (Functor(..))

Functos are everywhere!

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b

(*) For example, the partial application of the function constructor is unary and allows for a functor instance. Implement `fmap` for the instance

    `instance Functor ((->) t) where`

    and prove that the functor laws hold.

> instance Functor ((->) t) where
>   fmap = (.)

Weird, huh? The type of function composition --- `(.)` --- is exactly the type of `fmap` that we're looking for!

```
fmap :: (a -> b) -> ((->) t a) -> ((->) t b)
-- desugar the partial application of `(->)`
fmap :: (a -> b) -> (t -> a) -> (t -> b)
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

We can reason as follows to prove the appropriate functor laws.

```
  fmap id g
= (.) id g
= id . g
= g
= id g
```
``` 
  fmap (f . g) h
= (.) (f . g) h
= (f . g) . h
= f . (g . h)
= (.) f ((.) g h)
= fmap f (fmap g h)
= (fmap f . fmap g) h
```

(*) What's the type of `fmap fmap fmap` and what does this type mean? What's the type of `fmap . fmap`? Give an exemplary usage of `fmap . fmap`.

Let's check the types first!

```
fmap fmap fmap :: (Functor f, Functor f1) => (a -> b) -> f1 (f a) -> f1 (f b)
fmap (.) fmap :: Functor f => (a1 -> b) -> (a -> f a1) -> a -> f b
```

Okay, these type seems quite complex, so let's start a bit more slowly. The type of `fmap` itself is quite simple; since we need to use the function three times, let's write down three versions, where the type variables do not clash.

```
fmap :: Functor f => (a -> b) -> f a -> f b
fmap :: Functor g => (c -> d) -> g c -> g d
fmap :: Functor h => (x -> y) -> h x -> h y
```

Now let's apply `fmap` as function to `fmap`.
First we unify the function argument `(a -> b)` with `(c -> d) -> g c -> g d`.

```
  unify( a -> b , (c -> d) -> g c -> g d )
= { a |-> (c -> d) ; b |-> (g c -> g d) }
```

Using this substitution we add the second functor constraint and adapt the remaining argument accordingly.

```
fmap fmap :: (Functor f, Functor g) => f (c -> d) -> f (g c -> g d)
```

We end up with a function that expects a functor `f` containing a function and lifts both, the function argument and result, into the second functor `g`.

Last but not least, we add another `fmap` call. Due to the associativity of function application the the expression reads `(fmap fmap) fmap`, that is, we apply the function we ended up above to `fmap`.
That is, we need to unify `f (c -> d)` and `(x -> y) -> h x -> h y`.
In order for this unification to succeed, the underlying function `f` has to be the instance `((->) (x -> y))`.

```
  unify( f (c -> d) , (x -> y) -> h x -> h y )
= unify( f (c -> d) , (->) (x -> y) (h x -> h y) )
= { f |-> ((->) (x -> y)) ; c |-> (h x) ; d |-> (h y) }
```

Once again we use the resulting substitution in the remaining type signature `f (g c -> g d)`.

```
fmap fmap fmap :: (Functor g, Functor h) => (x -> y) -> g (h x) -> g (h y)
```

Since we explicitly instantiated one of the functors with `(->) t`, we only have two functor contexts in the type signature although we're using `fmap` three times.


How can we use this function now? We can apply it on nested functors to apply a function on the inner functor.

> instance Functor [] where
>   fmap = map
>
> instance Functor Maybe where
>   fmap _ Nothing = Nothing
>   fmap f (Just x) = Just (f x)

> test1 = fmap fmap fmap not [Just True, Nothing]
> test2 = fmap fmap fmap not (Just [True])

```
ghci> test1
[Just False,Nothing]
ghci> test2
Just [False]
```

(*) Define a lot of functor instances.

> data Zero a
>
> instance Functor Zero where
>   fmap f x = case x of

> data One a = One
>
> instance Functor One where
>   fmap f _ = One

> data Identity a = Identity a
>
> instance Functor Identity where
>   fmap f (Identity x) = Identity (f x)

> data Const a b = Const a
>
> instance Functor (Const a) where
>   fmap f (Const x) = Const x

> data Square a = Square a a
>
> instance Functor Square where
>   fmap f (Square x1 x2) = Square (f x1) (f x2)

> data Product a b = Product a b
>
> instance Functor (Product a) where
>   fmap f (Product x y) = Product x (f y)
