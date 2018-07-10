> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE FlexibleInstances #-}

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

> instance Profunctor (->) where
>   dimap f g h xC = g (h (f xC))
>
> instance Strong (->) where
>   first' f (xA,xQ) = (f xA, xQ)
>
> instance Choice (->) where
>   -- left' :: (a -> b) -> Either a q -> Either b q
>   left' fAB eAQ = either (\xA -> Left (fAB xA)) (\xQ -> Right xQ) eAQ

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
> testPrism :: PrismO a b
> testPrism = undefined
>
> instance Choice CConst where
>   left' (CConst xB) = CConst (Left xB)

> review :: Optic CConst s a -> a -> s
> review o val = unCConst (o (CConst val))
>  where unCConst (CConst x) = x

(1) Define the second auxilary function for lenses, namely `view`. Think about the needed profunctor --- let's call it `View` --- first.

> data View a b c = View { runView :: b -> a }
>
> instance Profunctor (View a) where
>   --dimap :: (d -> b) -> (c -> e) -> View a b c -> View a d e
>   dimap fDB gCE (View fBA) = View (\xD -> let xB = fDB xD
>                                               xA = fBA xB
>                                           in xA)
>
> instance Strong (View a) where
>   -- first' :: View a b c -> View a (b,q) (c,q)
>   first' (View fBA) = View (\(xB,xQ) -> fBA xB)
>
> -- Optic (View a) s a  ~ View a a a -> View a s s
> view :: Optic (View a) s a -> s -> a
> view o strct = runView (o (View id)) strct
    
Use the same idea to define `preview`.

> instance Choice (View (Maybe a)) where
>   -- left' :: View (Maybe a) b c -> View (Maybe a) (Either b q) (Either c q)
>   left' (View fBMA) = View (\eBQ -> either (\xB -> fBMA xB) (\xQ -> Nothing) eBQ)
>
> -- Optic (View (Maybe a) s a   ~ View (Maybe a) a a -> View (Maybe a) s s
> preview :: Optic (View (Maybe a)) s a -> s -> Maybe a
> preview o strct = runView (o (View Just)) strct

(3) In class we defined a prism and a lens in order to compose these two. As the simple composition was advertised as the big adventage of profunctor optics we we're quite confused, when this did not work as expected.
Implement the missing pieces in order to compose the following constructs and evaluate the exemplary calls `test1` to `test4` or argue why this is not possible.

> data Dimension = TwoD (Int,Int)
>                | ThreeD (Int,Int,Int)
>  deriving Show
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
> twoDHeight :: (Strong p, Choice p) => Optic p Dimension Int
> twoDHeight = twoDPrismO . pairLens
>
> -- test1 = view twoDHeight (TwoD (100,100))
> test2 = preview twoDHeight (TwoD (200,100))
> test2' = preview twoDHeight (ThreeD (200,100,0))
> -- test3 = review twoDHeight 200
> test4 = set twoDHeight (TwoD (100,100)) 200
> test4' = set twoDHeight (ThreeD (100,100,100)) 200


(1) Define `head` based on lenses/prisms.

> headL :: LensO [a] a
> headL = makeLens split unsplit
>  where split []     = (undefined,[])
>        split (x:xs) = (x,xs)
>        unsplit (x,xs) = x:xs

    unsplit (split [])
  = unsplit (undefined,[])
  = undefined : []
  = []

  [a] ~ (b,q)

  [] :: [a]       ~  (Nothing,[]) :: (Maybe a, [a])
  (x:xs) :: [a]   ~  (Just x,xs)  :: (Maybe a, [a])


  [a] ~ Either b q

  [] :: [a]       ~   Right ()        :: Either a ()
  (x:xs) :: [a]   ~   Left (x)     :: Either a ()
  
> safeHeadL :: LensO [a] (Maybe a)
> safeHeadL = makeLens split unsplit
>  where split []     = (Nothing,[])
>        split (x:xs) = (Just x,xs)
>        unsplit (mx,xs) = maybe xs (:xs) mx

    unsplit (split [])
  = unsplit (Nothing,[])
  = maybe [] (:[]) Nothing
  = []

     split (unsplit (mx,xs))
   = split (maybe xs (:xs) mx)
   (1)
   = split (maybe xs (:xs) Nothing)
   = split xs
   ...

> headP :: PrismO [a] a
> headP = makePrism match build
>  where match []     = Right ()
>        match (x:xs) = Left x
>        build eAB = either (\y -> [y]) (\() -> []) eAB


     build (match [])
   = build (Left ())
   = []

     build (match (x:xs))
   = build (Right x)
   = [x]

(2) Define `(!!)` based on lenses/prisms.

> idxL idx = undefined
> idxP idx = undefined

(3) Define `member` based on lenses/prisms.

> -- behaves like `member` on `Data.Set`
> containsL val = undefined
> containsP val = undefined

(4) Define `lookup` based on lenses/prisms.

> lookupL :: Eq a => a -> LensO [(a,b)] (Maybe b)
> lookupL val = makeLens (split val) unsplit
>  where split _ []     = (Nothing, [])
>        split v ((x,y):xs) | x == v = (Just y, xs)
>                           | otherwise = let (my,ys) = split v xs
>                                         in (my,(x,y):ys)
>        unsplit (mx,xs) = undefined
>
> lookupP val = undefined
