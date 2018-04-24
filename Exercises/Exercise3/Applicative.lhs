> import Prelude hiding (sequenceA)
> import Control.Applicative (Applicative(..))

Give implementations for the following functions using `Applicative`.

> sequenceA :: Applicative f => [f a] -> f [a]
> sequenceA = undefined

> -- don't reuse sequenceA here ; )
> sequenceA_ :: Applicative f => [f a] -> f ()
> sequenceA_ = undefined
 
> replicateA :: Applicative f => Int -> f a -> f [a]
> replicateA n = undefined
> 
> mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
> mapA f as = undefined
>
> filtering :: Applicative f => (a -> f Bool) -> [a] -> f [a]
> filtering p xs = undefined

Implement three example for usages of `filtering`.

Give at least one example for `filtering` that yields a list as result -- which applicative instance comes in handy here?

Consider the following data type `ZipList` that basically represents a focussed position with values to the left and right.

> data ZipList a = ZipList [a] a [a]

Implement a valid applicative instance for `ZipList`.
