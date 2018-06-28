
data Universal = Unit
               | Pair Universal Universal
               | This Universal
               | That Universal
  deriving (Eq, Show)

class Generic a where

  universal :: a -> Universal

genericEq :: Generic a => a -> a -> Bool
genericEq x y = universal x == universal y

instance Generic Bool where

  universal True  = This Unit
  universal False = That Unit

data Colour = Red | Green | Blue | Yellow

instance Generic Colour where

  universal Red    = This (This Unit)
  universal Green  = This (That Unit)
  universal Blue   = That (This Unit)
  universal Yellow = That (That Unit)

instance Generic a => Generic [a] where

  universal []     = This Unit
  universal (x:xs) = That (Pair (universal x) (universal xs))

instance Generic () where

  universal () = Unit

instance (Generic a, Generic b) => Generic (a,b) where

  universal (x,y) = Pair (universal x) (universal y)

instance (Generic a, Generic b) => Generic (Either a b) where

  universal (Left  x) = This (universal x)
  universal (Right y) = That (universal y)

serialize :: Generic a => a -> [Bool]
serialize = binary . universal

binary :: Universal -> [Bool]
binary Unit       = [False, False]
binary (Pair x y) = [False, True ] ++ binary x ++ binary y
binary (This x)   = [True,  False] ++ binary x
binary (That x)   = [True,  True ] ++ binary x 

data UniMap a = UniMap (Maybe a) (UniMap (UniMap a)) (UniMap a) (UniMap a)

emptyUniMap :: UniMap a
emptyUniMap = UniMap Nothing emptyUniMap emptyUniMap emptyUniMap

emptyGenMap :: Generic k => GenMap k a
emptyGenMap = GenMap emptyUniMap

lookupG :: Generic k => k -> GenMap k a -> Maybe a
lookupG k (GenMap m) = lookupUni (universal k) m

lookupUni :: Universal -> (UniMap a) -> Maybe a
lookupUni Unit (UniMap a b c d) = a
lookupUni (Pair x y) (UniMap a b c d) = lookupUni x b >>= lookupUni y
lookupUni (This x) (UniMap a b c d) = lookupUni x c
lookupUni (That x) (UniMap a b c d) = lookupUni x d

updateUni :: Universal -> (Maybe a -> Maybe a) -> UniMap a -> UniMap a
updateUni Unit       upd (UniMap a b c d) = UniMap (upd a) b c d
updateUni (Pair x y) upd (UniMap a b c d) = UniMap a 
   (updateUni x (Just . updateUni y upd . maybe emptyUniMap id) b) c d
updateUni (This x)   upd (UniMap a b c d) = UniMap a b (updateUni x upd c) d
updateUni (That x)   upd (UniMap a b c d) = UniMap a b c (updateUni x upd d)

insertG :: Generic k => k -> a -> GenMap k a -> GenMap k a
insertG k v (GenMap m) = GenMap (updateUni (universal k) (const (Just v)) m)

newtype GenMap k a = GenMap (UniMap a)
