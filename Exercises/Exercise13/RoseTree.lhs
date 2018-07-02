> {-# LANGUAGE RankNTypes #-}

In class we discussed continuation-based lists.
 
> import CMonoid

(1) Implement a function `fromRoseTree`, that folds a`RoseTree` into a continuation-based list.

> data RoseTree m a =
>     Leaf a
>   | Node (C m (RoseTree m a))
>
> fromRoseTree :: Monoid m => RoseTree m a -> C m a
> fromRoseTree = undefined

(2) Implement the `filterC` for continuation-based lists, which behaves as `filter` on ordinary lists.

> filterC :: Monoid m => (a -> Bool) -> C m a -> C m a
> filterC = undefined

(3) Implement the function`nth` continuation-based lists, which yields`Just` nth element of the list or `Nothing`. Use the data type `S` to define this function.

> newtype S a = S { getS :: Int -> Either Int a } 
>
> nth :: Int -> (forall m . Monoid m => C m a) -> Maybe a
> nth n f = undefined

(4) Implement a function `headC` for continuation-based lists, which yields `Just` the first element of a list or `Nothing`.

> newtype Head a = Head { getHead :: Maybe a }
>
> headC :: (forall m . Monoid m => C m a) -> Maybe a
> headC f = undefined
