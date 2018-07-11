> {-# LANGUAGE RankNTypes #-}

In class we discussed continuation-based lists.
 
> import CMonoid
> import Control.Monad (MonadPlus(..), guard)
> import Control.Applicative (Alternative(..))

(1) Implement a function `fromRoseTree`, that folds a`RoseTree` into a continuation-based list.

> data RoseTree m a =
>     Leaf a
>   | Node (C m (RoseTree m a))
>
> concatC :: Monoid m => C m (C m a) -> C m a
> concatC cs = cs >>= id
>
> fromRoseTree :: Monoid m => RoseTree m a -> C m a
> fromRoseTree (Leaf a)  = return a
> fromRoseTree (Node as) = concatC $ fmap fromRoseTree as

(2) Implement the `filterC` for continuation-based lists, which behaves as `filter` on ordinary lists.

> instance Monoid m => Alternative (C m) where
>   empty = mempty
>   a <|> _ = a
>                                              
> instance Monoid m => MonadPlus (C m) where
>   mzero = mempty
>   mplus = mappend
> 
> filterC  :: Monoid m => (a -> Bool) -> C m a -> C m a
> filterC p xs = do
>   x <- xs
>   guard (p x)
>   return x

(3) Implement the function`nth` continuation-based lists, which yields`Just` nth element of the list or `Nothing`. Use the data type `S` to define this function.

> newtype S a = S { getS :: Int -> Either Int a } 
>
> s :: a -> S a
> s x = S (\n -> if n == 0
>                then Right x
>                else Left (n - 1))
> 
> instance Monoid (S a) where
>   mempty = S Left
> 
>   S fx `mappend` S fy = S (\n -> case fx n of
>                                    Left  m -> fy m
>                                    Right x -> Right x)
> 
> nth :: Int -> (forall m . Monoid m => C m a) -> Maybe a
> nth n xs = case getS (fromC s xs) n of
>   Right x -> Just x
>   Left  _ -> Nothing

(4) Implement a function `headC` for continuation-based lists, which yields `Just` the first element of a list or `Nothing`.

> newtype Head a = Head { getHead :: Maybe a }
>
> instance Monoid (Head a) where
>   mempty = Head Nothing
> 
>   Head Nothing `mappend` hs = hs
>   hs           `mappend` _  = hs
> 
> headC :: (forall m . Monoid m => C m a) -> Maybe a
> headC xs = getHead $ fromC (Head . Just) xs
