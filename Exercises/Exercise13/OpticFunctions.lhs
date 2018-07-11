> {-# LANGUAGE RankNTypes #-}
> import Optics

Up to now we only discussed accessors for data types that are sum or product types in a quite obvious way.
In this exercise we like to take a look at some data types and functions on them that are sum and product types in disguise!
Define a lens/prism for the following functions and argue what's suited best (if any is preferable)!
The original functionality should then be accessed using `view` or `preview`.

For example, for `headL` we want to have the following behaviour.

   ```{.haskell}
   > view headL [1..10]
   1
   ```


(1) Define `head` based on lenses/prisms.

We cannot define a `Lens`, because there is no implementation in case of the
empty list, such that `split` and `unsplit` form an isomorphism.

> headL :: LensO [a] a
> headL = makeLens split unsplit
>  where split (x:xs) = (x,xs)
>     -- what to do for an empty list?
>        split []     = undefined
>        unsplit = undefined

   (1) unsplit (split xs) = xs
   
   (Case A) xs = []
     unsplit (split [])
   = unsplit undefined
   = undefined
   != []

Instead, we try to define `head` as prism.

> headP :: PrismO [a] a
> headP = makePrism match build
>  where match []     = Right []
>        match (x:xs) = Left x
>        build = either (\x -> [x]) id

   (1) build (match xs) = xs
   
   (Case A) xs = []
     build (match [])
   = build (Right [])
   = either (:[]) id (Right [])
   = []

   (Case B) xs = y:ys
     build (match (y:ys))
   = build (Left y)
   = either (:[]) id (Left y)
   = [y]
   != y:Ys

The prism we ended up with also does not fulfill the isomorphism property that we need
to form a valid lens.

There is an intuitive reason we cannot define a valid prism nor a valid lens for `head`.
The essence of a lens is that the structure at hand can be described as a product of two types,
and a prism describes structures that represents the sum of two types.
Now let's try to describe the list type `[a]` as a product or sum of two types.
Let's take a prism first: is there a type `q` such that we can describe `[a]` as a sum of
`a` and such a type `q`?
No matter what we choose, we will loose the remaining list as seen above in the definition of `match`.
Next up is a lens: is there a type `q` such that we can describe `[a]` as a product of
`a` and such a type `q`?
We can indeed choose the tail of the list for the second component, sucht that `q ~ [a]`.
However, in case of an empty list we do not have a value `a` at hand that we can use
for the first component.
We can overcome this burden by implementing a "safe" version of `head` as lens.

> safeHeadL :: LensO [a] (Maybe a)
> safeHeadL = makeLens split unsplit
>  where split (x:xs) = (Just x,xs)
>        split []     = (Nothing, [])
>        unsplit (mx, xs) = maybe xs (:xs) mx


(2) Define `(!!)` based on lenses/prisms.

The same reasoning applies for the function `(!!)` as it describes the same
relationship between lists and one of its elements.
The only difference is which element we acutally pick, that is, there
are more "failure" cases as for `head`.
That is, we need to project and insert the element from the right (index) position.

> idxL :: Int -> LensO [a] (Maybe a)
> idxL idx = makeLens (split idx) (unsplit idx)
>  where split _ []     = (Nothing, [])
>        split 0 (x:xs) = (Just x,xs)
>        split n (x:xs) = let (mxs,ys) = split (n-1) xs
>                         in (mxs,x:ys)
>        unsplit n (mx,xs) =
>          maybe xs (\x -> let (ys,zs) = (take n xs,drop n xs) in ys ++ [x] ++ zs) mx

(3) Define `member` based on lenses/prisms.

> -- behaves like `member` on `Data.Set`
> containsL val = undefined
> containsP val = undefined

(4) Define `lookup` based on lenses/prisms.

> -- behaves like `lookup`
> lookupL val = undefined
> lookupP val = undefined
