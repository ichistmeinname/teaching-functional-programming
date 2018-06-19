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

> type Field = Array Bool
> type Field2 = I.IntMap ()

Finally, you should compare your implementations with the variant on infinite lists.

Which data structure makes the race? In addition to your implementation, also provide test cases to benchmark your implementation easily as well as a table of the actual benchmarks for each implementation; the fastest implementation gets a chocolate bar!


> eratostenesArray :: Field -> Int -> Int -> [Int]
> eratostenesArray arr n upTo
>   | n > upTo  = []
>   | arr ! n   = n : eratostenesArray arr2 (n+1) upTo

> --  | arr ! n   = n : eratostenesArray (arr' (n^2)) (n+1) upTo
>   | otherwise = eratostenesArray arr (n+1) upTo
>  where
>    arr2 = foldl (\array i -> update array i (const False)) arr [n,2*n..upTo]
>    arr' i | i > upTo  = arr
>           | otherwise = update (arr' (i+n)) i (const False)
>
> initialField upTo = listToArray $ replicate (upTo + 2) True
>
> eratostenesIMap :: Field2 -> Int -> Int -> [Int]
> eratostenesIMap arr n upTo
>   | n > upTo  = []
>   | Just _ <- I.lookup n arr = n : eratostenesIMap arr2 (n+1) upTo
>   | otherwise = eratostenesIMap arr (n+1) upTo
>  where
>    arr2 = foldl (\array i -> I.delete i array) arr [n,2*n..upTo]
>    arr' i | i > upTo  = arr
>           | otherwise = I.delete i (arr' (i+n))
>
> initialField2 upTo = I.fromList $ zip [0..] (replicate (upTo + 2) ())

> eratostenesIMap2 :: I.IntMap Bool -> Int -> Int -> [Int]
> eratostenesIMap2 arr n upTo
>   | n > upTo  = []
>   | arr I.! n = n : eratostenesIMap2 arr2 (n+1) upTo
>   | otherwise = eratostenesIMap2 arr (n+1) upTo
>  where
>    arr2 = foldl (\array i -> I.update (const (Just False)) i array) arr [n,2*n..upTo]
>    arr' i | i > upTo  = arr
>           | otherwise = I.update (const (Just False)) i (arr' (i+n))
>
> initialField3 upTo = I.fromList $ zip [0..] (replicate (upTo + 2) True)
