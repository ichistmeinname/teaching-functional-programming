> import GenericProgramming
> import Test.QuickCheck
> import Data.List (elemIndex)

In the lecture we developed generic implementations for the equality check and for a serialisation function.

In Haskell it is possible to automatically derive instances for the type class `Ord`.
The basic scheme underlying this feature is that the outermost constructors will be compared
with respect to the order in which they were defined.
If the outermost constructors of both arguments match, their arguments will be matched
from left to right.

We define such a function by transforming the given values to our generic representation first
and compare them afterwards.

> genericCmp :: Generic a => a -> a -> Ordering
> genericCmp x y = compare (universal x) (universal y)

Hence, we need an `Ord`-instance for `Universal`s.

The derived `Ord`-instance would do it as well, but we define
the instance here by ourselves to make it more clear how the
constructors are compared.

> instance Ord Universal where
>   compare Unit Unit = EQ
>   compare Unit _    = LT
>   compare _    Unit = GT
> 
>   compare (This u) (This v) = compare u v
>   compare (This _) _        = LT
>   compare _        (This _) = GT
> 
>   compare (That u) (That v) = compare u v
>   compare (That _) _        = LT
>   compare _        (That _) = GT
> 
>   compare (Pair u1 v1) (Pair u2 v2) = compare [u1,v1] [u2,v2]

The crucial detail is that `This` is smaller than `That`, which results
in the wanted property that constructors are "ordered" with respect to the
order in which they were defined.
The last rule uses the `Ord`-instance for lists.

Now we check the wanted property using QuickCheck for different types.

> genCmpIsConsistent :: (Ord a, Generic a) => a -> a -> Bool
> genCmpIsConsistent x y = genericCmp x y == compare x y

> testGenericCmp :: IO ()
> testGenericCmp = do
>   quickCheck (genCmpIsConsistent :: Bool -> Bool -> Bool)
>   quickCheck (genCmpIsConsistent :: [Bool] -> [Bool] -> Bool)
>   quickCheck (genCmpIsConsistent :: Colour -> Colour -> Bool)
>   quickCheck (genCmpIsConsistent :: [[Colour]] -> [[Colour]] -> Bool)

Numbers are sorted in a lexicographical fashion: 0 is the smallest number,
follows by all negative numbers and positive numbers at last.
The absolute numbers (without leading sign) are compared lexicographically with
respect to their binary representation: beginning with the LSB.
