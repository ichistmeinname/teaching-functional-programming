> import Control.Monad.Plus (MonadPlus (..))
> import Data.Functor.Identity (Identity (..))

We introduced the type class `MonadPlus` in class and discussed its connection to non-determinism.
   
Define a function that computes all permutations of a given list non-deterministically.

> insert :: MonadPlus mp => a -> [a] -> mp [a]
> insert x []         = return [x]
> insert x yys@(y:ys) = return (x:yys) `mplus` fmap (y:) (insert x ys)

> permutations :: MonadPlus mp => [a] -> mp [a]
> permutations [] = return []
> permutations (x:xs) = permutations xs >>= insert x

Instead of using `insert`, we can define a more general function that computes an element and a remaining list for a given input list. Such a function decomposes the list.

> decompose :: MonadPlus mp => [a] -> mp (a, [a])
> decompose [] = mzero
> decompose (x:xs) = return (x,xs)
>            `mplus` do (y,ys) <- decompose xs
>                       return (y, x:ys)

λ> decompose [1,2,3] :: [(Int, [Int])]
[(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
λ> decompose [1,2,3] :: Maybe (Int, [Int])
Just (1, [2, 3])

Based on `decompose` we can compute the permutations of a list by inserting the element (the first component of the resulting pair) at the head of the remaining list (the second component).

> permutations' :: MonadPlus mp => [a] -> mp [a]
> permutations' [] = return []
> permutations' xs = do
>   (h,t) <- decompose xs
>   perm <- permutations' t
>   return (h : perm)

On top of that, you can implement a function `solve`, such that `solve num s` non-deterministically computes a list of of `n` natural numbers that sum up to `s`.

> solve :: MonadPlus mp => Int -> Int -> mp [Int]
> solve numbers | numbers < 0 = const mzero
>               | otherwise   = solver
>   where solver s | s <  0    = mzero
>                  | s == 0    = return (replicate numbers 0)
>                  | otherwise = do
>                      (x, _) <- decompose [0..s]
>                      xs     <- solve (numbers - 1) (s - x)
>                      return (x:xs)

We can also define the function explicitely by recursion and folding the potential arithmetic values non-deterministically.

> solve' :: MonadPlus mp => Int -> Int -> mp [Int]
> solve' num s | num < 0 = mzero
>              | s < 0   = mzero
>              | s == 0  = return (replicate num 0)
> solve' num s = do
>   n <- foldr mplus mzero (map return [0..s])
>   ns <- solve' (num-1) (s-n)
>   return (n:ns)

λ> solve (-3) 42 :: Maybe [Int]
Nothing
λ> solve (-3) 42 :: [[Int]]
[]
λ> solve 2 5 :: [[Int]]
[[0,5],[1,4],[2,3],[3,2],[4,1],[5,0]]
λ> solve 3 1 :: [[Int]]
[[0,0,1],[0,1,0],[1,0,0]]

Alternatively, we generate the solutions in ascending order and use `permutations` to compute the missing combinations.

> type Cmp a m = a -> a -> m Bool
>
> insertM :: Monad m => Cmp a m -> a -> [a] -> m [a]
> insertM _  x []         = return [x]
> insertM p  x yys@(y:ys) = do
>   b <- p x y
>   if b then return (x:yys)
>        else fmap (y:) (insertM p x ys)
> 
> insertSortM :: Monad m => Cmp a m -> [a] -> m [a]
> insertSortM _ []     = return []
> insertSortM p (x:xs) = do
>   ys <- insertSortM p xs
>   insertM p x ys
>
> cmpId :: Ord a => Cmp a Identity
> cmpId x y = Identity (x < y)
>
> cmpND :: MonadPlus m => Cmp a m
> cmpND x y = return True `mplus` return False

> sort :: Ord a => [a] -> [a]
> sort = runIdentity . insertSortM cmpId

> permute :: [a] -> [[a]]
> permute = insertSortM cmpND

λ> sort [42,15,4,31,17]
[4,15,17,31,42]
λ> permute [42,15,4] :: [[Int]]
[[42,15,4],[15,42,4],[15,4,42],[42,4,15],[4,42,15],[4,15,42]]
