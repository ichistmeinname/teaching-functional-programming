module SearchTree where

import Data.Maybe     ( isJust )
import Prelude hiding ( lookup )

data Tree k v = Empty | Node (Tree k v) k v (Tree k v)
  deriving (Eq, Show)

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
