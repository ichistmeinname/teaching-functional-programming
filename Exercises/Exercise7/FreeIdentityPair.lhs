> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE EmptyCase #-}
>
> import Data.Functor.Identity (Identity(..))
> import Free

> class Iso t1 t2 where
>     to :: t1 -> t2
>     from :: t2 -> t1

First, we want to search for the functor in order to get an instantiation of `Free f a` that is isomorphic to `Identity a`.

1. Give an implementation of the corresponding `Iso`-instance.

The key observation here is that in order to have a data type that is isomorphic to `Identity` we only need one constructor, since `Identity` also has only one constructor.
We already discussed "funny looking" functors in the previous weeks, one that now comes pretty handy is the `Zero` data type: a functor that has no constructors.

> data Zero a
>
> instance Functor Zero where
>     fmap f z = case z of

In order to work with `Zero` we need the language extension `EmptyCase`.
With this option enables, we can pattern match on a value of type `Zero a` and "call the bluff": i.e., realise that could not have been a value in the first place, because there is not constructor to use.

> instance Iso (Free Zero a) (Identity a) where
>     to (Pure x) = Identity x
>     to (Impure fx) = case fx of
>     from (Identity x) = Pure x

2. Prove the round-trip property for `to` and `from`.

We reason as follows.

    (a) forall types (a :: *) and values (fx :: Free Zero a),

           from (to fx) = fx

      by induction on `fx`

      (1) fx = Pure x
      
        from (to (Pure x))
      = from (Identity x)
      = Pure x

      (2) fx = Impure z

      We make case destinction on `z` and realise that there is no constructor
        of type `Zero`. The equality thus holds trivially.


    (b) forall types (a :: *) and values (ix :: Identity a),

           to (from ix) = ix

      by induction on `ix`

      (1) ix = Identity x
      
        to (from (Identity x))
      = to (Pure x)
      = Identity x


Second, we search for the monad (aka. data type that is an instance of `Monad`) that is isomorphic to `Free Pair a`, where `Pair` is defined as follows.

> data Pair a = Pair a a

3. Give the functor instance for `Pair`.

> instance Functor Pair where
>     fmap f (Pair x y) = Pair (f x) (f y)


4. Implement the `Iso`-instance for `Free Pair a` and the corresponding monad.

Here we can observe that using `Pair` as functor gives us the possibility to represent two constructor.
`Pure` gives rise to a constructor with one polymorphic argument.
`Impure` then uses `Pair` as underlying functor, thus, we have a value `Impure (Pair fx fy)` where `fx, fy` are of type `Free Pair a` again.
That is, by using `Impure` we gain a constructor that has two polymorphic components.
Of course, this reminds us of `Tree`s!

> data Tree a = Leaf a | Node (Tree a) (Tree a)

> instance Iso (Free Pair a) (Tree a) where
>     to (Pure x) = Leaf x
>     to (Impure (Pair fx fy)) = Node (to fx) (to fy)
>     from (Leaf x) = Pure x
>     from (Node t1 t2) = Impure (Pair (from t1) (from t2))


5. Prove the round-trip property.

We reason as follows.

    (a) forall types (a :: *) and values (fx :: Free Tree a),

           from (to fx) = fx

      by induction on `fx`

      (1) fx = Pure x
      
        from (to (Pure x))
      = from (Leaf x)
      = Pure x

      (2) fx = Impure px

      We make case destinction on `px`.

          fx = Impure (Pair fy fz)  and induction hypothesis `from (to fy) = fy` and `from (to fz) = fz` in place

        from (to (Impure (Pair fy fz)))
      = from (Node (to fy) (to fz))
      = Impure (Pair (from (to fy)) (from (to fz))
      = Impure (Pair fy fz)

    (b) forall types (a :: *) and values (tx :: Tree a),

           to (from tx) = tx

      by induction on `tx`

      (1) tx = Leaf x
      
        to (from (Leaf x))
      = to (Pure x)
      = Leaf x

      (2) tx = Node t1 t2  and induction hypothesis `to (from t1) = t1` and `to (from t2) = t2` in place

        to (from (Node t1 t2)
      = to (Impure (Pair (from t1) (from t2)))
      = Node (to (from t1)) (to (from t2))
      = Node t1 t2
