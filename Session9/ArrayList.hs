module ArrayList where

import Prelude hiding (replicate)
import qualified Prelude as P (replicate)

type ArrayList a = [Bit a]

data Bit a = Zero | One (BinTree a)
  deriving Show

data BinTree a = Leaf a | BinTree a :+: BinTree a
  deriving Show

replicateLin :: Int -> a -> ArrayList a
replicateLin n = listToArrayList . P.replicate n

replicate :: Int -> a -> ArrayList a
replicate n x = repl n (Leaf x)
 where
  repl n t | n == 0    = emptyArrayList
           | even n = Zero  : aux 
           | odd n  = One t : aux
   where
    aux = repl (n `div` 2) (t :+: t)

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
