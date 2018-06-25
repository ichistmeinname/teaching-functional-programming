Extend your dequeue implementation from Execise 9 to an indexed dequeue,
which also allows indexed access to each dequeue element.
Implement the following interface.

For the data type and the implementation you should start with the list-based
 version of the Dequeue and replace the lists by array lists.

To obtain an efficient implementation, it might be useful to to store some addtional information.
Argue about the runtime complexity of each function.

> import Prelude hiding (null, length, head, last, tail, init, reverse, replicate)
> import qualified Prelude   as P
> import qualified ArrayList as AL

> data IdxDequeue a = IDQ Int Int (AL.ArrayList a) (AL.ArrayList a)
>   deriving Show
> 
> -- O(1)
> empty :: IdxDequeue a
> empty = IDQ 0 0 AL.empty AL.empty
> 
> -- O(1)
> null :: IdxDequeue a -> Bool
> null = (== 0) . length
> 
> -- O(1)
> length :: IdxDequeue a -> Int
> length (IDQ sx sy _ _) = sx + sy
> 
> -- amort. O(1)
> head :: IdxDequeue a -> a
> head (IDQ 0 0 _  _) = error "IdxDequeue.head: empty queue"
> head (IDQ 0 _ _ ys) = AL.first ys
> head (IDQ _ _ xs _) = AL.first xs
> 
> -- amort. O(1)
> last :: IdxDequeue a -> a
> last = head . reverse
> 
> -- amort. O(1)
> (<.) :: a -> IdxDequeue a -> IdxDequeue a
> x <. IDQ sx 0  xs ys = IDQ 1        sx (x AL.<: ys) xs
> x <. IDQ sx sy xs ys = IDQ (sx + 1) sy (x AL.<: xs) ys
> 
> -- amort. O(1)
> (.>) :: IdxDequeue a -> a -> IdxDequeue a
> xs .> x = reverse (x <. reverse xs)
> 
> -- amort. O(1)
> tail :: IdxDequeue a -> IdxDequeue a
> tail (IDQ sx sy xs ys)
>   | sx == 0   = empty
>   | sx >= 2   = IDQ (sx - 1) sy (AL.rest xs) ys
>   | otherwise = IDQ (q + r)  q  (AL.fromList $ P.reverse vs) (AL.fromList us)
>   where (q , r ) = sy `quotRem` 2
>         (us, vs) = splitAt q $ AL.toList ys
> 
> -- amort. O(1)
> init :: IdxDequeue a -> IdxDequeue a
> init = reverse . tail . reverse
> 
> -- O(1)
> reverse :: IdxDequeue a -> IdxDequeue a
> reverse (IDQ sx sy xs ys) = IDQ sy sx ys xs
> 
> -- O(log (n))
> replicate :: Int -> a -> IdxDequeue a
> replicate n x
>   | n < 1     = empty
>   | otherwise = IDQ (n - 1) 1 (AL.replicate (n - 1) x) (AL.replicate 1 x)
> 
> -- O(|xs|)
> fromList  :: [a] -> IdxDequeue a
> fromList xs = foldr (<.) empty xs
> 
> -- O(|xs| + |ys|)
> toList :: IdxDequeue a -> [a]
> toList (IDQ _ _ xs ys) = AL.toList xs ++ P.reverse (AL.toList ys)
> 
> -- O(log (|xs| + |ys|))
> (!) :: IdxDequeue a -> Int -> a
> IDQ sx sy xs ys ! n = case getIndex sx sy n of
>   Nothing        -> error $ "IdxDequeue.(!): bad index " ++ show n
>   Just (Left  i) -> xs AL.! i
>   Just (Right j) -> ys AL.! j
> 
> -- O(log (|xs| + |ys|)
> modify :: Int -> (a -> a) -> IdxDequeue a -> IdxDequeue a
> modify n f (IDQ sx sy xs ys) = case getIndex sx sy n of
>   Nothing        -> error $ "IdxDequeue.modify: bad index " ++ show n
>   Just (Left  i) -> IDQ sx sy (AL.modify i f xs) ys
>   Just (Right j) -> IDQ sx sy xs (AL.modify j f ys)
> 
> -- O(1)
> getIndex :: Int -> Int -> Int -> Maybe (Either Int Int)
> getIndex sx sy n | n <= 0 || n >= total = Nothing
>                  | n < sx               = Just $ Left n
>                  | otherwise            = Just $ Right $ total - n - 1
>   where total = sx + sy
>

We can define a Foldable instance for array lists using `foldr`

> instance Foldable ArrayList where
>   foldr _ x0 Empty         = x0
>   foldr f x0 (NonEmpty tl) = foldr f x0 tl
> 
> instance Foldable TreeList where
>   foldr f x0 (Single x) = f x x0
>   foldr f x0 xs         = f y (foldr f x0 ys)
>     where (y, ys) = decons xs
> 
> toList :: ArrayList a -> [a]
> toList = foldr (:) []

The structures of type `[]` enable us to define `fmap` efficiently.
Note that we shouldn't use `foldr` as reconstructing each constructor of the array list
comes with a prize.

> instance Functor ArrayList where
>   fmap _ Empty         = Empty
>   fmap f (NonEmpty tl) = NonEmpty $ fmap f tl
> 
> instance Functor TreeList where
>   fmap f (Single x) = Single $ f x
>   fmap f (x :< xs)  = fmap f x :< fmap (\(x', y) -> (f x', f y)) xs
> 
> instance Functor Bit where
>   fmap _ Zero    = Zero
>   fmap f (One x) = One $ f x
