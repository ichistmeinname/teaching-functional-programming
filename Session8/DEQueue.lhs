> import Prelude hiding ( null, head, tail, last, init, reverse )
> import qualified Prelude as P

Extend the `Queue` data type from the lecture to a data type `Dequeue` for
a so called double ended queue with the following interface.

Hence, it should be possible to add and remove elements on both sides of the dequeue.
 
All operations are supposed to have amortised constant run time.
For this, use the same representation as for queues, but the following symmetric invariant.

    If one of the two lists is empty, the other list contains at most one element!

As for `Queue` we model our data type with two lists, where the the second one
contains the elements in reserved order.

> data Dequeue a = Dequeue [a] [a]
>  deriving Show

> empty :: Dequeue a
> empty = Dequeue [] []
>
> null :: Dequeue a -> Bool
> null (Dequeue xs ys) = P.null xs && P.null ys

> head :: Dequeue a -> a
> head (Dequeue [] [y]) = y
> -- the invariant forbids this constellation
> -- head (Dequeue [] ys ) = ...
> head (Dequeue (x:_) _) = x

> last :: Dequeue a -> a
> last (Dequeue [x] []) = x
> last (Dequeue _ (y:_)) = y

> infixr 5 <.,.>
>
> (<.) :: a -> Dequeue a -> Dequeue a
> -- special case of rule (2)
> -- x <. Dequeue [] [] = Dequeue [x] []
> -- special case of last rule
> -- x <. Dequeue [] ys = Dequeue [x] ys                   -- rule 2
> -- invariant says that `xs` has at most one element, so
> -- reverse xs == xs
> x <. Dequeue xs [] = Dequeue [x] xs
> x <. Dequeue xs ys = Dequeue (x:xs) ys

> (.>) :: Dequeue a -> a -> Dequeue a
> Dequeue [] ys .> x = Dequeue ys [x]
> Dequeue xs ys .> x = Dequeue xs (x:ys)

> tail1 :: Dequeue a -> Dequeue a
> tail1 (Dequeue xs []) = empty
> tail1 (Dequeue [] ys) = empty
> tail1 (Dequeue (x:xs@(_:_)) ys) = Dequeue xs ys
> tail1 (Dequeue [x] ys) = Dequeue (P.reverse tailYs) [headYs]
>  where
>    tailYs = P.tail ys
>    headYs = P.head ys

> init1 :: Dequeue a -> Dequeue a
> init1 (Dequeue xs []) = empty
> init1 (Dequeue [] ys) = empty
> init1 (Dequeue xs (y:ys@(_:_))) = Dequeue xs ys
> init1 (Dequeue xs [y]) = Dequeue [headXs] (P.reverse tailXs)
>  where
>    tailXs = P.tail xs
>    headXs = P.head xs

> tail :: Dequeue a -> Dequeue a
> tail (Dequeue xs []) = empty
> tail (Dequeue [] ys) = empty
> tail (Dequeue (x:xs@(_:_)) ys) = Dequeue xs ys
> tail (Dequeue [x] ys) = Dequeue (P.reverse vs) us
>  where
>    (us,vs) = splitAt (length ys `div` 2) ys

> init :: Dequeue a -> Dequeue a
> init (Dequeue xs []) = empty
> init (Dequeue [] ys) = empty
> init (Dequeue xs (y:ys@(_:_))) = Dequeue xs ys
> init (Dequeue xs [y]) = Dequeue us (P.reverse vs)
>  where
>    (us,vs) = splitAt (length xs `div` 2) xs

  init (init (init (init (init (1 <. 2 <. 3 <. 4 <. 5 <. empty)))))
= init (init (init (init (init (1 <. 2 <. 3 <. 4 <. Dequeue [5] [])))))
= init (init (init (init (init (1 <. 2 <. 3 <. Dequeue [4] [5])))))
= init (init (init (init (init (1 <. 2 <. 3 <. Dequeue [4] [5])))))
...
= init (init (init (init (init (Dequeue [1,2,3,4] [5])))))
= init (init (init (init (Dequeue [1,2] [4,3]))))           -- 4 + 2 + 2 = 8 steps
= init (init (init (Dequeue [1,2] [3])))
= init (init (Dequeue [1] [2]))                           -- 2 + 1 + 1 = 4 steps
= init (Dequeue [] [1])                               -- 1 + 1 = 2 steps
= Dequeue [] []

  8 + 4 + 2 = 14


  4 + 2 + 1 = 7

  n + n/2 + n/4 + n/8 .... <= 2n
