module ArrayListSafe where

-- Invariants of ArrayLists:
-- 1. All occuring binary trees are complete
-- 2. In position i the binary tree contains 2^i leafs
-- 3. The last Tree contains a One

data ArrayList a = Empty
                 | NonEmpty (TreeList a)
  deriving Show

data Bit a = Zero | One a
  deriving Show

data TreeList a = Single a
                | Bit a :< TreeList (a,a)  -- nested data type
  deriving Show

infixr 4 :<, <:

empty :: ArrayList a
empty = Empty

isEmpty :: ArrayList a -> Bool
isEmpty Empty = True
isEmpty _     = False

(<:) :: a -> ArrayList a -> ArrayList a
x <: Empty        = NonEmpty (Single x)
x <: (NonEmpty l) = NonEmpty (cons x l)

cons :: a -> TreeList a -> TreeList a
cons x (Single y)    = Zero :< Single (x,y)
cons x (Zero :< xs)  = One x :< xs
cons x (One y :< xs) = Zero :< cons (x,y) xs -- polymorphic recursion
  
listToArrayList :: [a] -> ArrayList a
listToArrayList = foldr (<:) empty

first :: ArrayList a -> a
first (NonEmpty (Single x)) = x
first (NonEmpty l) = fst $ decons l

rest ::  ArrayList a -> ArrayList a
rest (NonEmpty (Single _)) = Empty
rest (NonEmpty l) = NonEmpty $ snd $ decons l

decons :: TreeList a -> (a, TreeList a)
decons (One x :< xs)           = (x , Zero :< xs)
decons (Zero  :< Single (x,y)) = (x,Single y)
decons (Zero  :< xs)           = 
      let ((x,y),ys) = decons xs in
        (x, One y :< ys)


(<!!) :: ArrayList a -> Int -> a
Empty <!! _ = error "ArrayList.<!!: empty list"
NonEmpty l <!! n = select 1 sel l n
  where 
    sel x m | m == 0 = x
            | otherwise = error $ "ArrayList.<!!: invalid index " ++ show n

select :: Int -> (b -> Int -> a) -> TreeList b -> Int -> a
select size_t sel (Single x) n  = sel x n
select size_t sel (bit :< xs) n =
  case bit of
    Zero -> select (size_t*2) sel' xs n
    One x -> if n < size_t then
               sel x n
             else
               select (size_t*2) sel' xs (n - size_t)

 where --sel' :: (a,a) -> Int -> a
       sel' (l,r) m | m < size_t = sel l m
                    | otherwise  = sel r (m - size_t)  

(<!) :: ArrayList a -> Int -> a
Empty <! _ = error "ArrayList.<!: empty list"
NonEmpty l <! n = at l n

at :: TreeList a -> Int -> a
at (Single x) 0 = x
at (Single _) n = error "ArrayList.<!: index out of bound"
at (Zero :< bs) n = if even n then x else y 
    where (x,y) = at bs (n `div` 2)
at (One x :< bs) 0 = x
at (One x :< bs) n = if odd n then x else y 
    where (x,y) = at bs ((n - 1) `div` 2)

modify :: Int -> (a -> a) -> ArrayList a -> ArrayList a
modify _ _ Empty = error "ArrayList.modify: empty list"
modify n f (NonEmpty l) = NonEmpty $ update 1 upd l n
  where 
    upd x m | m == 0 = f x
            | otherwise = error $ "ArrayList.modify: invalid index " ++ show n

update :: Int -> (a -> Int -> a) ->  TreeList a -> Int -> TreeList a
update size_t upd (Single x)  n = Single (upd x n)
update size_t upd (bit :< xs) n =
  case bit of
    Zero -> Zero :< update (size_t*2) upd' xs n
    One x -> if n < size_t then
               One (upd x n) :< xs
             else
               One x :< update (size_t*2) upd' xs (n - size_t)
 where upd' (l,r) m | m < size_t = (upd l m, r)
                    | otherwise  = (l, upd r (m - size_t))   

modify' :: Int -> (a -> a) -> ArrayList a -> ArrayList a
modify' _ _ Empty = error "ArrayList.modify: empty list"
modify' n f (NonEmpty l) = NonEmpty $ update' n f l

update' :: Int -> (a -> a) -> TreeList a -> TreeList a
update' 0 f (Single x) = Single (f x)
update' _ _ (Single _) = error "ArrayList.modify': index oout of bound"
update' n f (Zero :< bs) = Zero :< update' (n `div` 2) f' bs
     where f' (x,y) = if even n then (f x,y) else (x,f y)
update' 0 f (One x :< bs) = One (f x) :< bs
update' n f (One x :< bs) = One x :< update' ((n-1) `div` 2) f' bs
     where f' (x,y) = if odd n then (f x,y) else (x,f y)

replicate :: Int -> a -> ArrayList a
replicate 0 _ = Empty
replicate n x = NonEmpty $ repl n x

repl :: Int -> a -> TreeList a
repl 1 x = Single x
repl n x = (if even n then Zero else One x) :< repl (n `div` 2) (x,x)

instance Foldable ArrayList where
  foldr _ x0 Empty         = x0
  foldr f x0 (NonEmpty tl) = foldr f x0 tl

instance Foldable TreeList where
  foldr f x0 (Single x) = f x x0
  foldr f x0 xs         = f y (foldr f x0 ys)
    where (y, ys) = decons xs

toList :: ArrayList a -> [a]
toList = foldr (:) []
