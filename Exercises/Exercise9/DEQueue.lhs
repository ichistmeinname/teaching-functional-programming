> import Prelude hiding ( null, head, tail, last, init )
> import qualified Prelude as P

Extend the `Queue` data type from the lecture to a data type `Dequeue` for
a so called double ended queue with the following interface.

> data Dequeue a
> 
> empty :: Dequeue a
> empty = undefined
>
> null :: Dequeue a -> Bool
> null = undefined
>
> head :: Dequeue a -> a
> head = undefined
>
> last :: Dequeue a -> a
> last = undefined
>
> (<.) :: a -> Dequeue a -> Dequeue a
> (<.) = undefined
>
> (.>) :: Dequeue a -> a -> Dequeue a
> (.>) = undefined
>
> tail :: Dequeue a -> Dequeue a
> tail = undefined
>
> init :: Dequeue a -> Dequeue a
> init = undefined

Hence, it should be possible to add and remove elements on both sides of the dequeue.
 
All operations are supposed to have amortised constant run time.
For this, use the same representation as for queues, but the following symmetric invariant.

    If one of the two lists is empty, the other list contains at most one element!

The problematic cases are `tail` and `init`.
Here you should be careful to avoid several inefficient steps for different nested executions
of `Dequeue` operations.
Argue by means of a good example, why your implementation performs n `Dequeue` operations
 in an overall run time of O(n).
