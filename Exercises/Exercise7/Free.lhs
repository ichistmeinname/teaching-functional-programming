> module Free where
>
> data Free f a = Pure a
>               | Impure (f (Free f a))

> instance Functor f => Functor (Free f) where
>     fmap f (Pure a) = Pure (f a)
>     fmap f (Impure fx) = Impure (fmap (fmap f) fx)
>
> instance Functor f => Applicative (Free f) where
>     pure = Pure
>     Pure f <*> fx = fmap f fx
>     Impure ff <*> fx = Impure (fmap (\ff' -> ff' <*> fx) ff)
>
> instance Functor f => Monad (Free f) where
>     return = Pure
>     Pure x >>= f = f x
>     Impure fx >>= f = Impure (fmap (\fx' -> fx' >>= f) fx)
