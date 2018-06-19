data Tree a = L a | F (Tree a) (Tree a)

data FullTree a = Leaf a | Fork (FullTree (a,a))
  deriving Show

data NonFullTree a = NonLeaf a | NonFork (NonFullTree a, NonFullTree a)
  deriving Show

-- leaves enumerated from 1 to 2^height
fullTree :: Int -> FullTree Int
fullTree = mkTree 1
 where
  mkTree n 0      = Leaf n
  mkTree n height =
    let d' = height - 1 in
    fork (mkTree n d') (mkTree (2^d' + n) d')

-- smart constructor
-- partially defined
fork :: FullTree a -> FullTree a -> FullTree a
fork (Leaf x)  (Leaf y)  = Fork $ Leaf (x,y)
fork (Fork t1) (Fork t2) = Fork $ fork t1 t2
