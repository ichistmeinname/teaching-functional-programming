> {-# LANGUAGE RankNTypes #-}

> module Optics where
>
> import Data.List
> import       Data.Maybe

As an alternative to the direct encoding we also discussed the approach using profunctor optics to generalise lenses and prisms.

> class Profunctor p where
>   dimap :: (c -> a) -> (b -> d) -> p a b -> p c d
>
> iso :: Profunctor p => (s -> a) -> (a -> s) -> p a a -> p s s
> iso = dimap
>
> type Optic p s a = p a a -> p s s
> type LensO s a = forall p. Strong p => Optic p s a
> type PrismO s a = forall p. Choice p => Optic p s a
>
> class Profunctor p => Strong p where
>   first' :: p a b -> p (a,q) (b,q)
>
> class Profunctor p => Choice p where
>   left' :: p a b -> p (Either a q) (Either b q)

> makeLens :: (s -> (a,q)) -> ((a,q) -> s) -> LensO s a
> makeLens split unsplit = iso split unsplit . first'
>
> makePrism :: (s -> Either a q) -> (Either a q -> s) -> PrismO s a
> makePrism match build = iso match build . left'
>
> instance Profunctor (->) where
>    -- dimap :: (c -> a) -> (b -> d) -> (a -> b) -> (c -> d)
>   dimap f g h xC = g (h (f xC))
>
> instance Strong (->) where
>   first' f (xA,xQ) = (f xA, xQ)
>
> modify :: Optic (->) s a -> (a -> a) -> s -> s
> modify o f stct = o f stct
>
> set :: Optic (->) s a -> s -> a -> s
> set o stct val = modify o (const val) stct

> data CConst a b = CConst b

> instance Profunctor CConst where
>   dimap f g (CConst xB) = CConst xD
>    where xD = g xB
>

> review :: Optic CConst s a -> a -> s
> review o val = unCConst (o (CConst val))
>  where unCConst (CConst x) = x


(1) Define the second auxilary function for lenses, namely `view`. Think about the needed profunctor --- let's call it `View` --- first.

> data View a b c = View { runView :: b -> a }
>
> instance Profunctor (View a) where
>   dimap f _ (View vf) = View (vf . f)
>
> instance Strong (View a) where
>   first' (View vf) = View (vf . fst)
>
> view :: Optic (View a) s a -> s -> a
> view o = runView (o (View id))
    
Use the same idea to define `preview`.

> data Preview a b c = Preview { runPreview :: b -> Maybe a }

> instance Profunctor (Preview a) where
>   dimap f _ (Preview vf) = Preview (vf . f)
>
> instance Choice (Preview a) where
>   left' (Preview vf) = Preview (either vf (const Nothing))

> preview :: Optic (Preview a) s a -> s -> Maybe a
> preview o = runPreview (o (Preview Just))

If we allowed `FlexibleInstances`, we could just reuse `View` to define `Preview` as it is just a special case: `Preview a b c = View (Maybe a) b c`.

(3) In class we defined a prism and a lens in order to compose these two. As the simple composition was advertised as the big adventage of profunctor optics we we're quite confused, when this did not work as expected.
Implement the missing pieces in order to compose the following constructs and evaluate the exemplary calls `test1` to `test4` or argue why this is not possible.

> data Dimension = TwoD (Int,Int)
>                | ThreeD (Int,Int,Int)
>
> twoDPrismO :: PrismO Dimension (Int,Int)
> twoDPrismO = makePrism match2D build2D
>  where
>   match2D dmn = case dmn of
>                   TwoD   hw  -> Left hw
>                   ThreeD hwd -> Right hwd
>   build2D = either TwoD ThreeD
>
> pairLens :: LensO (a,b) a
> pairLens = makeLens id id
>
> twoDHeight :: (Choice p, Strong p) => Optic p Dimension Int
> twoDHeight = twoDPrismO . pairLens
>

For `test1` we need that `View a` is an instance of `Choice`, which is not possible to implement.

> -- instance Choice (View a) where
> --   left' (View vf) = View (either vf (\_ -> xA))
> --    where xA = undefined

The expression `test2` works without additional effort, because we already implemented
a `Choice`-instance for `Preview`.    

For `test3' we need to implement a `Strong`-instance for `CConst`, which is, again, not possible.

> -- instance Strong CConst where
> --   first' (CConst v) = CConst (v,q)
> --    where q = undefined
>
> instance Strong (Preview a) where
>   first' (Preview vf) = Preview (vf . fst)

In order to execute `test4` we need to implement a `Choice`-instance for `(->)`.

> instance Choice (->) where
>   left' f = either (Left . f) Right

> -- test1 = view twoDHeight (TwoD (100,100))
> test2 = preview twoDHeight (TwoD (100,100))
> -- test3 = review twoDHeight 200
> test4 = set twoDHeight (TwoD (100,100)) 200

In the end we can observe that we can only use the prism-view function, because we cannot
not know for sure that the component is available, since we're using a prism on the way.
However, we cannot construct a value using `review`, because we cannot invent the missing
components (i.e. the second component of the dimension pair), but we can modify this component
for a specific value of that constructor `TwoD` using `set`.
