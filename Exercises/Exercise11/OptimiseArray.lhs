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
>
> at :: A.ArrayList a -> Int -> (A.BinTree a -> A.BinTree a) -> a
> at []         _ _   = error "index out of bounds"
> at (A.Zero :bs) n get = at bs (n     `div` 2) $ get . side (even n)
> at (A.One x:_ ) 0 get = leaf $ get x
> at (A.One _:bs) n get = at bs ((n-1) `div` 2) $ get . side (odd n)
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

Implement the function `replicate :: Int -> a -> ArrayList` a for the safe implementation of array-lists. The run-time complexity should be as efficient as discussed in the last execise for the unsafe implementation.

> replicate :: Int -> a -> AS.ArrayList a
> replicate 0 _ = AS.Empty
> replicate n x = AS.NonEmpty $ repl n x
> 
> repl :: Int -> a -> AS.TreeList a
> repl 1 x = AS.Single x
> repl n x = (if even n then AS.Zero else AS.One x) AS.:<
>                        repl (n `div` 2) (x,x)
