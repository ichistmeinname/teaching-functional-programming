> import Prelude hiding (Functor(..))

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b
>
> class Applicative f where
>   (<*>) :: f (a -> b) -> f a -> f b
>   pure :: a -> f a
>
> class Monad m where
>   (>>=) :: m a -> (a -> m b) -> m b
>   return :: a -> m a
