> import Prelude hiding (Monoid(..))

First up we define a `Monoid`{.haskell} type class with functions `one`{.haskell} for the neutral element and `(.*.)`{.haskell} for the associative operation.

> class Monoid m where
>    (.*.) :: m -> m -> m
>    one :: m

There are ways to declare a monoid instance for integer values, for example the monoid for additation and muliplication.

> data Sum = Sum Integer
>
> instance Monoid Sum where
>   Sum x .*. Sum y = Sum (x + y)
>   one             = Sum 0
>
> data Prod = Prod Integer
>
> instance Monoid Prod where
>   Prod x .*. Prod y = Prod (x * y)
>   one               = Prod 1

There are also two instances for Boolean values.

> data Or = Or Bool
> data And = And Bool
>
> instance Monoid Or where
>   Or b1 .*. Or b2 = Or (b1 || b2)
>   one             = Or False
>
> instance Monoid And where
>   And b1 .*. And b2 = And (b1 && b2)
>   one               = And True

And many more!

> instance Monoid [a] where
>   (.*.) = (++)
>   one   = []
>
> instance Monoid () where
>   (.*.) _ _ = ()
>   one       = ()
>
> newtype Func a = Func (a -> a)
>
> instance Monoid (Func a) where
>   one  = Func id
>   Func f .*. Func g = Func (f . g)

Arbitrary functions of type `a -> b` cannot be a monoid, since we cannot "invent" a value of type `b` given a value of type `a` for the neutral element.

Another example that does _not_ fulfill the laws is subtraction on `Integer.

> data Diff = Diff Int
>
> instance Monoid Diff where
>   Diff x .*. Diff y = Diff (x - y)
>   one               =  Diff 0


Let's focus on maybe and lists now.

We can define an instance for `Maybe` that prefers the first valid value that occurs.

> data First a = First (Maybe a)
>
> instance Monoid (First a) where
>   one                   = First Nothing
>   First Nothing   .*. x = x
>   First (Just r)  .*. _ = First (Just r)

As an alternative we can take the last occurrence.

> data Last a = Last (Maybe a)
>
> instance Monoid (Last a) where
>   one                  = Last Nothing
>   x  .*. Last Nothing  = x
>   _  .*. Last (Just r) = Last (Just r)

Last but not least, we can also consider a special instance if the underlying element is a monoid as well!

> instance Monoid a => Monoid (Maybe a) where
>   one =  Nothing
>   Nothing .*. x      = x
>   Just r .*. Nothing = Just r
>   Just r .*. Just s  = Just (r .*. s)

Note that the `(.*.)` occurring in the third rule is of type `a -> a -> a`.

We can use a similar idea for lists by using zip.

> data Zip a = Zip [a]
>
> instance Monoid a => Monoid (Zip a) where
>   Zip xs .*. Zip ys = Zip (zipWith (.*.) xs ys)
>   one               = Zip (repeat one)

We need to define an infinite list as neutral element, because `zip` stops if one of its arguments runs out of elements, i.e., the list is empty. Since we do not know the length of the given list beforehand, we need to be equipped for all situations, thus, need infinite many elements.


In the end, we want to look at some use-cases for monoids in action. Folding a list of monoids is as straightforward as follows.

> mfold :: Monoid m => [m] -> m
> mfold = foldr (.*.) one

The same holds for trees, if we cosidere the following folding function for trees first.

> data Tree a = Empty | Leaf a | Node (Tree a) (Tree a)
> 
> foldTree :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
> foldTree f _ e Empty      = e
> foldTree f _ e (Leaf   x) = f x
> foldTree f g e (Node l r) = foldTree f g e l `g` foldTree f g e r

> mfoldTree :: Monoid m => Tree m -> m
> mfoldTree = foldTree id (.*.) one

For trees without the `Empty`-constructor, we wouldn't even need `one` here.
