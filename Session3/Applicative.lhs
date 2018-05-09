> data Identity a = Identity a
>   deriving Show
> 
> instance Functor Identity where
>     fmap f (Identity x) = Identity (f x)
> 
> instance Applicative Identity where
>     pure = Identity
>     Identity f <*> Identity x = Identity (f x)
> 
>
> filtering :: Applicative f => (a -> f Bool) -> [a] -> f [a]
> filtering p xs =
>   -- foldr (\(x :: a) (acc :: f [a]) ->
>   foldr (\x acc ->
>             (\b -> if b then (x:) else id) <$> p x <*> acc)
>         (pure [])
>         xs
>              -- fmap (\b -> if b then x else []) (p x :: f Bool) )

Implement three example for usages of `filtering`.

λ> filtering (Identity . even) [4..6]
Identity [4,6]

λ> filtering (\a -> if a > 13 then Nothing else Just (a <= 7)) [4..9]
Just [4,5,6,7]

λ> filtering (\a -> if a > 13 then Nothing else Just (a <= 7)) [4..14]
Nothing

Give at least one example for `filtering` that yields a list as result -- which applicative instance comes in handy here?

λ> filtering (>) [4..12] 8
[9,10,11,12]

filtering :: Applicative f => (a -> f Bool) -> [a] -> f [a]
(>) :: (Ord a) => a -> a -> Bool

Which applicative do we need to use to make the above example a valid expression?

Let's say `f = ((->) Bool)`

filteringWithF :: (a -> ((->) Bool) Bool) -> [a] -> f [a]
filteringWithF :: (a -> (Bool -> Bool) -> [a] -> f [a]

Let's try again : )
Let's say `f = ((->) b)`

filteringWithF :: (a -> (((->) b) Bool)) -> [a] -> f [a]
filteringWithF :: (a -> (b -> Bool)) -> [a] -> f [a]
filteringWithF :: (a -> (b -> Bool)) -> [a] -> (b -> [a])
filteringWithF :: (a -> b -> Bool) -> [a] -> b -> [a]
       
filtering (>) [4..12] :: (Ord a, Num a) => a -> [a]
filtering (>) [4..12] 8 :: (Ord a, Num a) => [a]
