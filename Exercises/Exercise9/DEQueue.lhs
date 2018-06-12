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

For an empty `Dequeue` both lists have to be empty.
Thus, the `null`-predicate needs to check for both lists as the other list
could contain an element (see invariant).

> empty :: Dequeue a
> empty = Dequeue [] []
>
> null :: Dequeue a -> Bool
> null (Dequeue xs ys) = P.null xs && P.null ys

Because of the invariant, `head` yields the first element of the first list,
or --- if it's empty --- the only element of the second one.
Since the second list can only have one in element in that case, it does not
matter that the elements are in reserved order.

> head :: Dequeue a -> a
> head (Dequeue []    [y]) = y
> head (Dequeue (x:_) _  ) = x

The definition of `last` works analogue.

> last :: Dequeue a -> a
> last (Dequeue _   (y:_)) = y
> last (Dequeue [x] _    ) = x

So, `head` and `last` obviously have a constant run time with respect to the
number of elements in the `Dequeue`.

Once again, we have two operations that work in a symmetric fashion:
adding an element to the front and adding an element to the back.

For adding to the front, if the second list is empty, the first one
has at most one element, so we can add the given element `x` as singleton
for the first list and the preceeding first list as second list.
Since the list `xs` has at most one element, it is once again safe to use
it as second list.

> infixr 5 <.,.>
>
> (<.) :: a -> Dequeue a -> Dequeue a
> x <. Dequeue xs [] = Dequeue [x] xs
> x <. Dequeue xs ys = Dequeue (x:xs) ys
>
> (.>) :: Dequeue a -> a -> Dequeue a
> Dequeue [] ys .> y = Dequeue ys [y]
> Dequeue xs ys .> y = Dequeue xs (y:ys)

Once again, it's is trivial to see that these operations have a constant run time
with respect to the number of elements in the `Dequeue`.

The problematic cases are `tail` and `init`.
Here you should be careful to avoid several inefficient steps for different nested executions
of `Dequeue` operations.

Now we come to the more involved function definitions.
The function `tail` removes the first element of the `Dequeue`.
In case of an empty first list and one element in the second list,
we yield the empty list.
When the first list has at least two elements, we can safely
remove the first element of that list.
Last, we need to consider the case the first list has only one element.
If the second list is empty, we can just yield the empty list.
However, in all other cases, we need to split the elements between
both lists.
Note that the elements that move from the second to the first list need
to come from the back end of the second list!
When deciding on the splitting method, we should keep in mind that
`init` behaves in the symmetric way.
That is, it is not clever to end up in a situation where only one element
of the second list is moved to the first one or the other way around.
Instead, we want to split the lists evenly!
 
> tail :: Dequeue a -> Dequeue a
> tail (Dequeue [] [_]) = empty
> tail (Dequeue (_:xs@(_:_)) ys) = Dequeue xs ys
> tail (Dequeue [_] []) = empty
> tail (Dequeue [_] ys) = Dequeue (P.reverse vs) us
>  where (us,vs) = splitAt (length ys `div` 2) ys
>
> init :: Dequeue a -> Dequeue a
> init (Dequeue xs  (_:y:ys))  =  Dequeue xs (y:ys)
> init (Dequeue [_] []      )  =  empty
> init (Dequeue xs  [_]     )  =  Dequeue us (P.reverse vs)
>   where (us,vs) = splitAt (length xs `div` 2) xs

Due to splitting and reversing one half of the split, in the worst-case, these function have a linear run rime with
respect to the number of elements.
However, since we split the list in the middle, we have a constant amortised run time, since
the overall run time for arbitrary sequences of operations is linear with respect to the number of elements.

Argue by means of a good example, why your implementation performs n `Dequeue` operations
 in an overall run time of O(n).

Let's take a look at the following example.

      init (init (init (init (init ((1 :: Int) <. 2 <. 3 <. 4 <. 5 <. empty)))))
    = init (init (init (init (init (Dequeue [1,2,3,4] [5])))))
    = init (init (init (init (Dequeue [1,2] [4,3]))))    -- linear (4 steps)
    = init (init (init (Dequeue [1,2] [3])))
    = init (init (Dequeue [1] [2]))                      -- linear (2 steps)
    = init (Dequeue [] [1])                              -- linear (1 steps)
    = empty

In general, the run time of a sequence of `init` and `(<.)` calls is at most

   n + n/2 + n/4 + n/8 + ... <= 2n,  where `n` is the number of elements

, since, in the worst-case, the length of the `Dequeue` is cut into half.

If you're interested in a proof of the run time, you can take a look at the following publication.

   "The design of functional programs: a calculational approach"
   by Robert Richard Hoogerwoord (Section 7: Two sided list operations).


An alternative version uses a function `reverse` on `Dequeue`s that has constant run time
with respect to the number of elements.

In order to reverse a `Dequeue` we merely need to swap both lists.

> reverse :: Dequeue a -> Dequeue a
> reverse (Dequeue xs ys) = Dequeue ys xs

Then we can define `last`, `(.>)` and `init` by means of `reverse` as follows.

> last' :: Dequeue a -> a
> last' = head . reverse
> 
> (..>) :: Dequeue a -> a -> Dequeue a
> xs ..> x = reverse (x <. reverse xs)
> 
> init' :: Dequeue a -> Dequeue a
> init' = reverse . tail . reverse
