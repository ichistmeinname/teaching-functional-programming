In the lecture we developed a second safe implementation for lookup and modify for array lists.
The advantage of this implementation is the fact, that it is no neccessary to substract the
number of skipped elements from the lookup index, as in the versions discussed before.
This last algorithm constructed the functions for the nested data type simply by distinguishing,
whether the searched index is even or odd. Transfer this algorithm to the non-safe implementation.
Compare the run-time of all implementations for accessing and modifying large array lists.

> import Prelude hiding (replicate)
> import qualified Prelude as P (replicate)
> import qualified ArrayList     as A
> import qualified ArrayListSafe as AS

We start with selectors that simplify the following code.

> leaf :: A.BinTree a -> a
> leaf (A.Leaf x) = x

> left :: A.BinTree a -> A.BinTree a
> left (l A.:+: _) = l

> right :: A.BinTree a -> A.BinTree a
> right (_ A.:+: r) = r

> (<!) :: A.ArrayList a -> Int -> a
> [] <! _ = error "ArrayList.<!: empty list"
> xs <! n = at xs n id

Instead of using a size-parameter in the implementation of `at`, we use a function `get`
that "describes" how to reach a leaf.
That is, depending on the index we modify the `get`-function to walk down the tree according
to the parity of the index.

> at :: A.ArrayList a -> Int -> (A.BinTree a -> A.BinTree a) -> a
> at []             _ _   = error "index out of bounds"
> at (A.Zero  : bs) n get = at bs (n     `div` 2) (get . side (even n))
> at (A.One x : _)  0 get = leaf (get x)
> at (A.One _ : bs) n get = at bs ((n-1) `div` 2) (get . side (odd n))
>
> side :: Bool -> (A.BinTree a -> A.BinTree a)
> side b = if b then left else right

Expression for testing purposes.

> fromList :: [a] -> A.ArrayList a
> fromList = foldr (A.<:) A.emptyArrayList
>
> constant = 50
> main = let test m n = foldr (+) 0 $ map (fromList [0..n] <!) $ P.replicate m n
>        in print $ test (constant * 5000000) 5000000
>

We compiled the programs with `ghc --make` (test cases 1 and 2) and `ghc -O2 --make` (test cased 3 to 5), respectively.
The test cases 1 and 3 used `constant = 1`, 2 and 4 used `constant = 2` and only the last one used `constant = 50`.

times in seconds
------------------

|Testfall             |    1   |    2    | Differenz (von 2 und 1)|   3    |    4    |    5     |
|---------------------|--------|---------|------------------------|--------|---------|----------|
|ArrayList            | 11.778 | 20.273  | 8.495                  | 2.794  | 2.902   |  12.908  |
|ArrayList (Optimiert)| 18.734 | 34.928  | 16.194                 | 2.791  | 2.901   |  12.779  |
|ArrayList (Safe)     | 15.708 | 29.014  | 13.306                 |


Implement the function `replicate :: Int -> a -> ArrayList` a for the safe implementation of array-lists. The run-time complexity should be as efficient as discussed in the last execise for the unsafe implementation.

We just use the same idea presented in the last group session.

> replicate :: Int -> a -> AS.ArrayList a
> replicate 0 _ = AS.Empty
> replicate n x = AS.NonEmpty $ repl n x
> 
> repl :: Int -> a -> AS.TreeList a
> repl 1 x = AS.Single x
> repl n x = (if even n then AS.Zero else AS.One x) AS.:<
>                        repl (n `div` 2) (x,x)
