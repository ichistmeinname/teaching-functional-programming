> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeOperators #-}

> import Prelude hiding (sequenceA)
> import Control.Applicative (Applicative(..))

Give implementations for the following functions using `Applicative`.

> sequenceA :: Applicative f => [f a] -> f [a]
> sequenceA = foldr mcons (pure [])
>   where mcons p q = pure (:) <*> p <*> q
> 
> sequenceA_ :: Applicative f => [f a] -> f ()
> sequenceA_ = foldr (\ e acc -> pure (flip const) <*> e <*> acc) (pure ())
> 
> replicateA :: Applicative f => Int -> f a -> f [a]
> replicateA n = sequenceA . replicate n
> 
> mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
> mapA f as = sequenceA (map f as)
>
> filtering :: Applicative f => (a -> f Bool) -> [a] -> f [a]
> filtering p = foldr (\ x acc -> (\ b -> if b then (x :) else id) <$> p x <*> acc) (pure [])
> 
> data Identity a = Identity a
>   deriving Show
> 
> instance Functor Identity where
>     fmap f (Identity x) = Identity (f x)
> 
> instance Applicative Identity where
>     pure = Identity
>     Identity f <*> Identity x = Identity (f x)
> 

Implement three example for usages of `filtering`.

λ> filtering (Identity . even) [4..6]
ExactlyOne [4,6]

λ> filtering (\a -> if a > 13 then Nothing else Just (a <= 7)) [4..9]
Just [4,5,6,7]

λ> filtering (\a -> if a > 13 then Nothing else Just (a <= 7)) [4..14]
Nothing

Give at least one example for `filtering` that yields a list as result -- which applicative instance comes in handy here?

λ> filtering (>) [4..12] 8
[9,10,11,12]

Consider the following data type `ZipList` that basically represents a focussed position with values to the left and right.

> data ZipList a = ZipList [a] a [a]

Implement a valid applicative instance for `ZipList`.

> instance Functor ZipList where
>     fmap f (ZipList xs x ys) = ZipList (fmap f xs) (f x) (fmap f ys)
>
> instance Applicative ZipList where
>     pure x = ZipList (repeat x) x (repeat x)
>     ZipList fl f fr <*> ZipList l x r = ZipList (zipWith ($) fl l) (f x) (zipWith ($) fr r)


> data (f :+: g) a = Inl (f a)
>                  | Inr (g a)
>
> data (f :*: g) a = Prod (f a) (g a)

> data (f :.: g) a = Compose (f (g a))
>
> class Natural f g where
>     eta :: f a -> g a

> instance (Functor f, Functor g) => Functor (f :+: g) where
>     fmap f (Inl fx) = Inl (fmap f fx)
>     fmap f (Inr fx) = Inr (fmap f fx)

> instance (Applicative f, Applicative g, Natural g f) => Applicative (f :+: g) where
>    pure = Inr . pure
>    Inr ff <*> Inr fx = Inr (ff <*> fx)
>    Inl ff <*> Inl fx = Inl (ff <*> fx)
>    Inl ff <*> Inr fx = Inl (ff <*> eta fx)
>    Inr ff <*> Inl fx = Inl (eta ff <*> fx)

> instance (Functor f, Functor g) => Functor (f :.: g) where
>     fmap f (Compose ffx) = Compose (fmap (fmap f) ffx)

> instance (Applicative f, Applicative g) => Applicative (f :.: g) where
>     pure = Compose . pure . pure
>     Compose fffx <*> Compose ffx = Compose (pure (<*>) <*> fffx <*> ffx)
