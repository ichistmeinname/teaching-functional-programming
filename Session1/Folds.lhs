The following solution does not work for infinite lists (yet).

> --Klappt leider noch nicht mit unendlichen listen
> takeL1 :: Int -> [a] -> [a]
> takeL1 n as =
>    foldl (\xs x -> if length xs == n
>                      then xs else xs ++ [x]) [] as

Can we make it work using |zip| to pair any value with its index?

> takeL2 :: Int -> [a] -> [a]
> takeL2 n xs = foldl (\ xs (x,i) ->
>                     if i >= n then xs else xs ++ [x])
>               [] (zip xs [0..])

If |xs| is an infinite list, the call |zip xs [0..]| does not terminate,
   thus, folding with `foldl` does not terminate as well.

Unfortunately, we can only make it work if we're "cheating", and only zip
as many argument as demanded by take, that is, |n|.

> takeL3 :: Int -> [a] -> [a]
> takeL3 n xs = foldl (\ xs (x,i) ->
>                     if i >= n then xs else xs ++ [x])
>               [] (zip xs [0..n-1])
 
Furthermore, we can observe that when we cut off elements from |xs| by using
|zip| this way, that we can get rid of the if-then-else-expression.
>
> takeL4 :: Int -> [a] -> [a]
> takeL4 n xs = foldl (\ xs (x,_) -> xs ++ [x])
>                     [] (zip xs [0..n-1])


Next up, how can we define |foldl| using |foldr|?
The easiest way is to observe that |foldl| works from left to right
through the given list and needs a function with flipped arguments when
compared to |foldr|. That is, we need to reverse the incoming list and
flip the arguments for the given function |f|.
 
> foldlR1 :: (a -> b -> a) -> a -> [b] -> a
> foldlR1 f a bs = foldr (flip f) a (reverse bs)

However, that is also another way that does not compromises the given
list beforehand.

> foldlR2 :: (b -> a -> b) -> b -> [a] -> b
> foldlR2 f b as = foldr (\ a g x -> g (f x a)) id as b

Observe that the function we apply to |foldr| takes three arguments,
thus, we end up with a function as result of the folding operation!

-- foldr (\ a g x -> g ((+) x a)) :: (t2 -> t1) -> [t1] -> (t2 -> t2)

Let's recall the definition of |foldr| once again

> foldr' f e [] = e
> foldr' f e (x:xs) = f x (foldr' f e xs)

and evaluate the expression |foldR2 (+) 0| for the empty list.

> --   (foldr (\a g x -> g ((+) x a)) id []) 0
> -- = id 0
> -- = 0

Here we can also realise that the only sane function to supply is |id|,
in order to yield the neutral element |0| in case of the empty list.

How does this expression evaluate for a non-empty list |[1]|?

> --  (foldr (\a g x -> g ((+) x a)) id [1]) 0
> -- = (\a g x -> g ((+) x a)) 1
> --                           (foldr (\a g x -> g ((+) x a)) id [])
> --    0
> -- = (\a g x -> g ((+) x a)) 1
> --                           id
> --    0
> -- = (\ g x -> g ((+) x 1)) id
> --    0
> -- = (\ x -> id ((+) x 1))
> --    0
> -- = id ((+) 0 1)
> -- = (+) 0 1
> -- = 1

Last but not least, let's define `lengthL` pointfree!

> lengthL :: [a] -> Int
> lengthL = foldl (\b _ -> 1 + b) 0
> 
> -- const :: a -> b -> a
> lengthL1 :: [a] -> Int
> lengthL1 = foldl (\b -> const (1 + b)) 0
> 
> lengthL2 :: [a] -> Int
> lengthL2 = foldl (const . (+ 1)) 0
 

We did not discuss this, but this is definitely a pointfree
version that does _not_ increase readability! However, it's fun
to find out what's going on here "type-wise".

> (.:) :: (b -> c) -> (a1 -> a -> b) -> a1 -> a -> c
> (.:) = (.) . (.)
> 
> lengthLObscure :: [a] -> Int
> lengthLObscure = foldl ((+ 1) .: const) 0
