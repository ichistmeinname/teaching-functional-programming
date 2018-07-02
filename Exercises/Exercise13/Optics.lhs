> {-# LANGUAGE RankNTypes #-}

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

(1) Define the second auxilary function for lenses, namely `view`. Think about the needed profunctor --- let's call it `View` --- first.

> data View a b
> view :: Optic View s a -> s -> a
> view = undefined
    
Use the same idea to define `preview`.

> data Preview a b
> preview :: Optic Preview s a -> s -> Maybe a
> preview = undefined

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
> -- twoDHeight = twoDPrismO . pairLens
>
> -- test1 = viewO twoDHeight (TwoD (100,100))
> -- test2 = previewO twoDHeight (TwoD (100,100))
> -- test3 = reviewO twoDHeight 200
> -- test4 = setO twoDHeight (TwoD (100,100)) 200
