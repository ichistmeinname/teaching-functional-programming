In the lecture we developed a second safe implementation for lookup and modify for array lists.
The advantage of this implementation is the fact, that it is no neccessary to substract the
number of skipped elements from the lookup index, as in the versions discussed before.
This last algorithm constructed the functions for the nested data type simply by distinguishing,
whether the searched index is even or odd. Transfer this algorithm to the non-safe implementation.
Compare the run-time of all implementations for accessing and modifying large array lists.

> import qualified ArrayList     as A
> import qualified ArrayListSafe as AS

> (<!) :: A.ArrayList a -> Int -> a
> (<!) = undefined

Implement the function `replicate :: Int -> a -> ArrayList` a for the safe implementation of array-lists. The run-time complexity should be as efficient as discussed in the last execise for the unsafe implementation.

> replicate :: Int -> a -> AS.ArrayList a
> replicate = undefined
