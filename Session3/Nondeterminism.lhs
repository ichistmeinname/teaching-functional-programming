> import Data.Functor.Identity (Identity(..))
>
> class Monad mp => MonadPlus mp where
>    mplus :: mp a -> mp a -> mp a
>    mzero :: mp a
>
> instance MonadPlus [] where
>    mplus = (++)
>    mzero = []

> -- insert actually inserts the first arguments at every possible
> --  position in the second argument
> insert :: MonadPlus mp => a -> [a] -> mp [a]
> insert x [] = return [x]
> insert x (y:ys) =
>     return (x : y : ys) `mplus` fmap (y:) (insert x ys)

> permutations :: MonadPlus mp => [a] -> mp [a]
> permutations [] = return []
> permutations (x:xs) = permutations xs >>= \ ys -> insert x ys
>
> trueOrFalse :: [Bool]
> trueOrFalse = [True,False]

> type Cmp m a = a -> a -> m Bool
>
> insertM :: Monad m => Cmp m a -> a -> [a] -> m [a]
> insertM _ x [] = return [x]
> insertM p x (y:ys) = do
>   b <- p x y
>   if b then return (x:y:ys)
>        else fmap (y:) (insertM p x ys)
>
> insertionSortM :: Monad m => Cmp m a -> [a] -> m [a]
> insertionSortM p [] = return []
> insertionSortM p (x:xs) = do
>   insertionSortM p xs >>= \ys -> insertM p x ys

> justSort :: Ord a => [a] -> [a]
> justSort xs = runIdentity (insertionSortM cmpId xs)
>
> cmpId :: Ord a => Cmp Identity a
> cmpId x y = Identity (x < y)

> cmpND :: MonadPlus mp => Cmp mp a
> cmpND _ _ = return True `mplus` return False

> nowPermute :: [a] -> [[a]]
> nowPermute xs = insertionSortM cmpND xs
 
