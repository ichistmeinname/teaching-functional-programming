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

Then test your implementation for the following data structures.

     Array (simple implementation from the lecture)
     IntMap (Haskell library, improvement of the array implementation)

> type Field = Array Bool
> type Field2 = I.IntMap ()

Finally, you should compare your implementations with the variant on infinite lists.

Which data structure makes the race? In addition to your implementation, also provide test cases to benchmark your implementation easily as well as a table of the actual benchmarks for each implementation; the fastest implementation gets a chocolate bar!

> initialField upTo = listToArray $ replicate (upTo+2) True

> eratostenesArray :: Field -> Int -> Int -> [Int]
> eratostenesArray f n upTo
>   | n > upTo  = []
>   | f ! n     = n : eratostenesArray f' (n+1) upTo
>   | otherwise = eratostenesArray f' (n+1) upTo
>  where f' = foldl (\a i -> update a i (const False)) f [n,2*n..upTo]
>
> primes n = eratostenesArray (initialField n) 2 n

> initialField2 upTo = I.fromList $ zip [0..] (replicate (upTo+2) ())
>
> eratostenesIMap :: Field2 -> Int -> Int -> [Int]
> eratostenesIMap f n upTo
>   | n > upTo                = []
>   | I.lookup n f /= Nothing = n : eratostenesIMap f' (n+1) upTo
>   | otherwise               = eratostenesIMap f' (n+1) upTo
>   where f' = foldl (\a i -> I.delete i a) f [n,2*n..upTo]
>
> primes2 n = eratostenesIMap (initialField2 n) 2 n

> main = print $ length $ primes3 upTo

> primes3 :: Int -> [Int]
> primes3 n = take n (sieve [2..])

> sieve :: [Int] -> [Int]
> sieve (p:ns) = p : sieve (filter (\x -> x `mod` p /= 0) ns)
