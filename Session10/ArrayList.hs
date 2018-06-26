module ArrayList where

import Debug.Trace

type ArrayList a = [Bit a]

data Bit a = Zero | One (BinTree a)
  deriving Show

data BinTree a = Leaf a | BinTree a :+: BinTree a
  deriving Show

emptyArrayList :: ArrayList a
emptyArrayList = []

(<:) :: a -> ArrayList a -> ArrayList a
x <: xs = cons (Leaf x) xs

cons :: BinTree a -> ArrayList a -> ArrayList a
cons tree []                   = [One tree]
cons tree (Zero : al)          = One tree : al
cons tree (One otherTree : al) = Zero : cons (tree :+: otherTree) al 

listToArrayList :: [a] -> ArrayList a
listToArrayList = foldr (<:) emptyArrayList

data Direction = L | R

(<!) :: Show a => ArrayList a -> Int -> a
xs <! n = at xs n id

at :: Show a => ArrayList a -> Int -> (BinTree a -> BinTree a) -> a
at []           _ _   = error "index out of bounds"
at (Zero  : bs) n get = at bs n' get'
 where
  n' = n `div` 2
  get' = get .  side (even n) -- (if even n then left else right)
at (One t : _)  0 get = leaf (get t)
at (One t : bs) n get = at bs n' get'
 where
  n'   = (n-1) `div` 2
  get' = get . side (odd n) -- (if odd n then left else right)

side b = if b then left else right

left :: BinTree a -> BinTree a
left (t1 :+: _) = t1

right :: BinTree a -> BinTree a
right (_ :+: t2) = t2

-- warning: partial selector
leaf :: BinTree a -> a
leaf (Leaf x) = x

-- (<!) :: ArrayList a -> Int -> a
-- Empty <! _ = error "ArrayList.<!: empty list"
-- NonEmpty l <! n = at l n

-- at :: TreeList a -> Int -> a
-- at (Single x) 0 = x
-- at (Single _) n = error "ArrayList.<!: index out of bound"
-- at (Zero :< bs) n = if even n then x else y 
--     where (x,y) = at bs (n `div` 2)
-- at (One x :< bs) 0 = x
-- at (One x :< bs) n = if odd n then x else y 
--     where (x,y) = at bs ((n - 1) `div` 2)



(<!!) :: ArrayList a -> Int -> a
xs <!! n =  select 1 xs n

select :: Int -> ArrayList a -> Int -> a
select size_t [] n           = error "access to non-existing index"
select size_t (Zero : al) n  = select (size_t*2) al n 
select size_t (One binTree : al) n
  | size_t > n = descend (size_t `div` 2) binTree n
  | otherwise  = select (size_t*2) al (n - size_t)

descend :: Int -> BinTree a -> Int -> a
descend 0 (Leaf x) 0         = x
descend size_l (tl :+: tr) n 
  | size_l > n = descend (size_l `div` 2) tl n
  | otherwise  = descend (size_l `div` 2) tr (n - size_l)
