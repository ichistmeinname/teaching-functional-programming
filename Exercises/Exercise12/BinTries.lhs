> import qualified Tries as T
> import Data.List (sortBy)
> import Data.Ord (comparing)
> import Control.Monad (mplus)

> data Bin = Tip | Bin Bin String Bin

The corresponding `BinMap` is defined as follows.

> data BinMap a = BinMap (Maybe a) (BinMap (T.StringMap (BinMap a)))

We see the following correspondences with respect to the data type `Bin`:
the constructor `Tip` is a nullary constructor, thus, we do not need
a further nesting of `BinMap`, but us a `Maybe a` only.
The second constructor `Bin2` has three arguments: a `Bin`, a String and
another `Bin`. The corresponding representation in `BinMap` uses nested
calls of `BinMap`, `StringMap` and another `BinMap`.

Now we can define the wanted functions.

> emptyBin :: BinMap a
> emptyBin = BinMap Nothing emptyBin

The function `emptyBin` creates an infinite tree without entries.

> lookupBin :: Bin -> BinMap a -> Maybe a
> lookupBin Tip         (BinMap a _) = a
> lookupBin (Bin l s r) (BinMap _ b) =
>   lookupBin l b >>= T.lookupString s >>= lookupBin r

The function `lookupBin` yields the potential value given a `Tip`,
and uses nested calls to `lookupX` for a `Bin`, thus, we're
branching with respect to the outer constructor of the key.

> updateBin :: Bin -> (Maybe a -> Maybe a) -> BinMap a -> BinMap a
> updateBin Tip         upd (BinMap a b) = BinMap (upd a) b
> updateBin (Bin l s r) upd (BinMap a b) =
>   BinMap a (updateBin l
>             (Just . T.updateString s
>                      (Just . updateBin r upd
>                            . maybe emptyBin id)
>                   . maybe T.emptyStringMap id)
>             b)

The function `updateBin` branches with respect to the key as well.
In case of `Tip` we're applying the given function `upd` with the
first argument of the `BinMap`.
In case of `Bin` we have, once again, nesting calls corresponding
to the `Bin` arguments.
Here, the calls of `Just` and `maybe` make sure that we're the inner
`update` functions work on `Maybe`-values.

In order to convert a `StringMap` into a list with key value pairs,
we need to construct the keys as we walk down a path in the map.
The following definition uses list comprehensions.

> stringMapToList :: T.StringMap a -> [(String,a)]
> stringMapToList (T.StringMap a b) =
>   maybe [] (\x -> [("",x)]) a ++
>   [ (c:cs,x) | (c,m) <- b, (cs,x) <- stringMapToList m ]

First, we yield the value corresponding to an empty string, if it exists,
and recreate the chars of the underlying `CharMap` recursively to get
the entries of the other keys.

Implement a monoid instance for the type `TreeMap a`, where `mplus` combines two `TreeMap` structures.
Try to implemet this function efficiently.
If both `TreeMap`s contain an entry, the value of the first `TreeMap` should be preferred.

In order to merge two `TreeMap`s in to one `TreeMap`, we need an auxiliary helper function
that uses a function that can combine elements of keys that exist in both `TreeMap`s.

> instance Monoid (T.TreeMap a) where
>   mempty  = T.emptyTreeMap
>   mappend = combineTreeMaps const

We use this additional arguments to combine nested `TreeMap`s recursively.

> combineTreeMaps :: (a -> a -> a)
>                 -> T.TreeMap a -> T.TreeMap a -> T.TreeMap a
> combineTreeMaps cmb (T.TreeMap a1 b1) (T.TreeMap a2 b2) =
>   T.TreeMap (combineStringMaps cmb a1 a2)
>           (combineTreeMaps (combineTreeMaps cmb) b1 b2)

Now, we need a function to combine `StringMap`s.

> combineStringMaps :: (a -> a -> a)
>                   -> T.StringMap a -> T.StringMap a -> T.StringMap a
> combineStringMaps cmb (T.StringMap a1 b1) (T.StringMap a2 b2) =
>   T.StringMap (combineRoots a1 a2)
>               (combineCharMaps (combineStringMaps cmb) b1 b2)
>  where
>   combineRoots (Just x) (Just y) = Just $ cmb x y
>   combineRoots u        v        = u `mplus` v

The local function `combineRoots` combines labels of the roos, if
there's a value in both arguments.
Otherwise we use `mplus` for `Maybe`-values to compute the new label.

Last but not least, we combine `CharMap`s as follows.

> combineCharMaps :: (a -> a -> a)
>                 -> T.CharMap a -> T.CharMap a -> T.CharMap a
> combineCharMaps cmb xs ys =
>   merge (sortBy (comparing fst) xs) (sortBy (comparing fst) ys)
>  where
>   merge []           l2           = l2
>   merge l1           []           = l1
>   merge ((c1,x1):l1) ((c2,x2):l2)
>     | c1 == c2  = (c1, cmb x1 x2) : merge l1           l2
>     | c1 <  c2  = (c1,x1)         : merge l1           ((c2,x2):l2)
>     | otherwise = (c2,x2)         : merge ((c1,x1):l1) l2

In order to avoid duplicates of keys, we sort the given `CharMap`s first
and combine them in ascending order.
