{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Tries where

type CharMap a = [(Char,a)]

emptyCharMap :: CharMap a
emptyCharMap = []

lookupChar :: Char -> CharMap a -> Maybe a
lookupChar _ [] = Nothing
lookupChar c ((c',v):cs) | c == c' = Just v
                         | otherwise = lookupChar c cs

insertChar :: Char -> a -> CharMap a -> CharMap a
insertChar c x xs = (c,x) : deleteChar c xs

deleteChar :: Char -> CharMap a -> CharMap a
deleteChar c = filter ((c /=) . fst)

-- data String = Nil | Cons Char String

data StringMap a =
  StringMap (Maybe a) (CharMap (StringMap a))
  deriving Show

emptyStringMap :: StringMap a
emptyStringMap = StringMap Nothing []

lookupString :: String -> StringMap a -> Maybe a
lookupString ""     (StringMap a _) = a
lookupString (c:cs) (StringMap _ b) =
   lookupChar c b >>= lookupString cs

insertString :: String -> a -> StringMap a -> StringMap a
insertString ""     x (StringMap a b) = StringMap (Just x) b
insertString (c:cs) x (StringMap a b) = StringMap a
   (insertChar c (insertString cs x 
                   (maybe emptyStringMap id (lookupChar c b))) b)

strings = ["eye","ear","even","tea","test","theater","time","easy"]

testTrie = foldr (uncurry insertString) emptyStringMap (zip strings [1..])
testTrie' = foldr (uncurry insertString') emptyStringMap (zip strings [1..])

deleteString :: String -> StringMap a -> StringMap a
deleteString ""     (StringMap _ b) = StringMap Nothing b
deleteString (c:cs) (StringMap a b) = StringMap a
   (maybe b (\m -> insertChar c (deleteString cs m) b) (lookupChar c b))

updateChar :: Char -> (Maybe a -> Maybe a) -> CharMap a -> CharMap a
updateChar c upd [] = maybe [] (\x -> [(c,x)]) (upd Nothing)
updateChar c upd ((c',x):xs)
  | c == c'   = maybe xs ( \y -> (c,y):xs) (upd (Just x))
  | otherwise = (c',x) : updateChar c upd xs

insertChar' :: Char -> a -> CharMap a -> CharMap a
insertChar' c x = updateChar c (const (Just x))

deleteChar' :: Char -> CharMap a -> CharMap a
deleteChar' c = updateChar c (const Nothing)

insertString' :: String -> a -> StringMap a -> StringMap a 
insertString' s x = updateString s (const (Just x))

deleteString' :: String -> StringMap a -> StringMap a
deleteString' s = updateString s (const Nothing)
updateString :: String -> (Maybe a -> Maybe a) -> StringMap a -> StringMap a
updateString ""     upd (StringMap a b) = StringMap (upd a) b
updateString (c:cs) upd (StringMap a b) = StringMap a
   (updateChar c (Just . updateString cs upd . maybe emptyStringMap id) b)

data Bin = IHi | O Bin | I Bin

data BinMap a = BinMap (Maybe a) (BinMap a) (BinMap a)

emptyBinMap :: BinMap a
emptyBinMap = BinMap Nothing emptyBinMap emptyBinMap

lookupBin :: Bin -> BinMap a -> Maybe a
lookupBin IHi     (BinMap a b c) = a
lookupBin (O bin) (BinMap a b c) = lookupBin bin b
lookupBin (I bin) (BinMap a b c) = lookupBin bin c

updateBin :: Bin -> (Maybe a -> Maybe a) -> BinMap a -> BinMap a
updateBin IHi     upd (BinMap a b c) = BinMap (upd a) b c
updateBin (O bin) upd (BinMap a b c) = BinMap a (updateBin bin upd b) c
updateBin (I bin) upd (BinMap a b c) = BinMap a b (updateBin bin upd c)


insertBin :: Bin -> a -> BinMap a -> BinMap a
insertBin bin x = updateBin bin (const (Just x))

deleteBin :: Bin -> BinMap a -> BinMap a
deleteBin bin = updateBin bin (const Nothing)

data Tree = Leaf String | Branch Tree Tree
  deriving Show

data TreeMap a = TreeMap (StringMap a) (TreeMap (TreeMap a))
  deriving Show

emptyTreeMap :: TreeMap a
emptyTreeMap = TreeMap emptyStringMap emptyTreeMap

lookupTree :: Tree -> TreeMap a -> Maybe a
lookupTree (Leaf str)     (TreeMap a b) = lookupString str a
lookupTree (Branch tl tr) (TreeMap a b) =
  lookupTree tl b >>= lookupTree tr

insertTreeSlow :: Tree -> a -> TreeMap a -> TreeMap a
insertTreeSlow (Leaf str)     x (TreeMap a b) = TreeMap (insertString str x a) b
insertTreeSlow (Branch tl tr) x (TreeMap a b) = TreeMap a
  (insertTreeSlow tl (insertTreeSlow tr x 
                   (maybe emptyTreeMap id (lookupTree tl b))) b)

updateTree :: Tree -> (Maybe a -> Maybe a) -> TreeMap a -> TreeMap a
updateTree (Leaf str) upd (TreeMap a b) = TreeMap (updateString str upd a) b
updateTree (Branch tl tr) upd (TreeMap a b) = TreeMap a
  (updateTree tl (Just . updateTree tr upd . maybe emptyTreeMap id) b)

insertTree :: Tree -> a -> TreeMap a -> TreeMap a
insertTree t v = updateTree t (const (Just v))

deleteTree :: Tree -> TreeMap a -> TreeMap a
deleteTree t = updateTree t (const Nothing)

tree :: Int -> Tree
tree 0 = Leaf "Hello"
tree 1 = Leaf "Students"
tree n = Branch (tree (n-1)) (tree (n-2))

testTreeTrie = foldr (\n trie -> insert (tree n) 73 trie) empty [1..10] 

class Trie t m | m -> t, t -> m where

  empty :: m a
  lookup :: t -> m a -> Maybe a 
  update :: t -> (Maybe a -> Maybe a) -> m a -> m a  

  insert :: t -> a -> m a -> m a
  insert k v = update k (const (Just v))

  delete :: t -> m a -> m a
  delete k = update k (const Nothing)

instance Trie Tree TreeMap where

  empty = emptyTreeMap

  lookup = lookupTree

  update = updateTree

instance Trie String StringMap where

  empty = emptyStringMap

  lookup = lookupString

  update = updateString

data T a = E | B (T a) (T a)
