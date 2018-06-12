module SearchTree where

import Observer
import Data.Maybe     ( isJust )
import Prelude hiding ( lookup )

data Tree k v = Empty | Node (Tree k v) k v (Tree k v)
  deriving (Eq, Show)

instance (Observe k, Observe v) => Observe (Tree k v) where
    obs Empty ref = o0 Empty "Empty" ref
    obs (Node t1 k v t2) ref = o4 Node "Node" t1 k v t2 ref

tree :: Tree String [Int]
tree = fromList
       [ ("Frank", [42])
       , ("Sascha", [-1])
       , ("Sandra", [4,8,15,16,23,42])
       ]

test1 :: IO ()
test1 = runO (print (lookup "Sandra" (observe "tree" tree)))
test2 :: IO ()
test2 = runO (print (lookup "Sandra" (observe "tree" tree)))
test3 :: IO ()
test3 = runO (print (lookup "Sascha" (observe "tree" tree)))
test4 :: IO ()
test4 = runO (print (lookup "Sasch" (observe "tree" tree)))

insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert key val Empty = Node Empty key val Empty
insert key val (Node l k v r) =
    case compare key k of 
        LT -> Node (insert key val l) k v r
        EQ -> Node l k val r
        _  -> Node l k v (insert key val r)

lookup :: Ord k => k -> Tree k v -> Maybe v
lookup key Empty = Nothing
lookup key (Node l k v r) =
    case compare key k of
        LT -> lookup key l
        EQ -> Just v
        _  -> lookup key r

contained :: Ord k => k -> Tree k v -> Bool
contained k = isJust . lookup k

delete :: Ord k => k -> Tree k v -> Tree k v
delete key Empty          = Empty
delete key (Node l k v r) =
    case compare key k of
        LT -> Node (delete key l) k v r
        EQ -> combine l r
        GT -> Node l k v (delete key r)

extractMin :: Tree k v -> (k, v, Tree k v)
extractMin Empty            = error "Split: empty tree"
extractMin (Node l k v r) | isEmpty l = (k, v, r)
                          | otherwise = let (k', v', l') = extractMin l
                                        in  (k', v', Node l' k v r)

combine :: Tree k v -> Tree k v -> Tree k v
combine t t' | isEmpty t' = t
             | otherwise  = Node t k v right
             where (k, v, right) = extractMin t'

isEmpty :: Tree k v -> Bool
isEmpty Empty = True
isEmpty _     = False

fromList :: Ord k => [(k, v)] ->  Tree k v
fromList = foldr (uncurry insert) Empty
