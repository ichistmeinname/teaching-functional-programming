> module Sieve where
>
> import Array
> import qualified Data.IntMap as I

In this excercise you are supposed to compare the array implementations from
the lecture implementing the Sieve of Eratosthenes.

To do this, define a function sieve, which is parameterised via the maximum entries
 of your array. Do not be too shy and test your implementations for numbers greater than 100000.

> upTo ::Int
> upTo = 100000

Then test your implementation for the following data structures:

Array (simple implementation from the lecture)
IntMap (Haskell library, improvement of the array implementation)

> type Field = Array ()
> type Field2 = I.IntMap ()

Finally, you should compare your implementations with the variant on infinite lists.

Which data structure makes the race? In addition to your implementation, also provide test cases to benchmark your implementation easily as well as a table of the actual benchmarks for each implementation; the fastest implementation gets a chocolate bar!


> eratostenesArray :: Field -> Int -> Int -> [Int]
> eratostenesArray = undefined
>
> eratostenesIMap :: Field2 -> Int -> Int -> [Int]
> eratostenesIMap = undefined
