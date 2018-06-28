> {-# LANGUAGE ExistentialQuantification #-}

In the lecture we discussed the following encoding of lenses and prisms.

> data Lens s a = forall q. Lens { split   :: s -> (a,q)
>                                , unsplit :: (a,q) -> s }

> data Prism s a = forall q. Prism { match :: s -> Either a q
>                                  , build :: Either a q -> s }

(1) Implement the homogenous composition between lenses and prisms, respectively.

> (|.|) :: Lens b c -> Lens a b -> Lens a c 
> (|.|) = undefined
>
> (&.&) :: Prism b c -> Prism a b -> Prism a c
> (&.&) = undefined

(2) Give two examples (that are not given in class) for prisms and lenses each and
compose these lenses/prisms using the combinators above.

We already discussed `view` and `set` as functions specific to lenses.
Typical functions used on prisms are `review` and `preview`.
There also combinators that work for both concepts: instead of setting a value,
we can also modify the underyling value using `over`.

(3) Give an implementation for `review` and `review`.

> preview :: Prism s a -> (s -> Maybe a)
> preview = undefined
>
> review :: Prism s a -> (a -> s)
> review = undefined

(4) Give an implementation for `overL` and `overP`.

> overL :: Lens s a -> (a -> a) -> (s -> s)
> overL = undefined
>
> overP :: Prism s a -> (a -> a) -> (s -> s)
> overP = undefined

(5) We want to restrict the return value by using the combinator `only`.
That is, if a given value is the one passed to `only`, then we want to be able to access that value,
and yield `()` otherwise to indicate the value is not present.
Is this combinator a lens, a prism, or both?

> onlyL :: Eq a => a -> Lens a ()
> onlyL = undefined
>
> onlyP :: Eq a => a -> Prism a ()
> onlyP = undefined
