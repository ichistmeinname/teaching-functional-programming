module GenericProgramming where

import Test.QuickCheck

data Universal = Unit
               | Pair Universal Universal
               | This Universal
               | That Universal
  deriving (Eq, Show)


{- 
In order to deserialise serialised data again, we added a function `fromUniversal` to the
`Generic` type class.
We also adapt the implemented instances.
-}

class Generic a where

  universal :: a -> Universal
  fromUniversal :: Universal -> a

genericEq :: Generic a => a -> a -> Bool
genericEq x y = universal x == universal y

instance Generic Bool where

  universal True  = This Unit
  universal False = That Unit

  fromUniversal (This Unit) = False
  fromUniversal (That Unit) = True


data Colour = Red | Green | Blue | Yellow
 deriving (Show, Ord, Eq)

instance Generic Colour where

  universal Red    = This (This Unit)
  universal Green  = This (That Unit)
  universal Blue   = That (This Unit)
  universal Yellow = That (That Unit)

  fromUniversal (This (This Unit)) = Red
  fromUniversal (This (That Unit)) = Green
  fromUniversal (That (This Unit)) = Blue
  fromUniversal (That (That Unit)) = Yellow


instance Generic a => Generic [a] where

  universal []     = This Unit
  universal (x:xs) = That (Pair (universal x) (universal xs))

  fromUniversal Unit        = []
  fromUniversal (Pair x xs) = fromUniversal x : fromUniversal xs


instance Generic () where

  universal () = Unit
  fromUniversal Unit = ()


instance (Generic a, Generic b) => Generic (a,b) where

  universal (x,y) = Pair (universal x) (universal y)
  fromUniversal (Pair u v) = (fromUniversal u, fromUniversal v)

instance (Generic a, Generic b) => Generic (Either a b) where

  universal (Left  x) = This (universal x)
  universal (Right y) = That (universal y)

  fromUniversal (This u) = Left  (fromUniversal u)
  fromUniversal (That u) = Right (fromUniversal u)


serialize :: Generic a => a -> [Bool]
serialize = binary . universal

binary :: Universal -> [Bool]
binary Unit       = [False, False]
binary (Pair x y) = [False, True ] ++ binary x ++ binary y
binary (This x)   = [True,  False] ++ binary x
binary (That x)   = [True,  True ] ++ binary x 

{-
Similary to `seralize` we use an auxiliary function `fromBinary`
to define the corresponding `deseralize` function.
The function `fromBinary` is basically is simple parser.
-}

deserialize :: Generic a => [Bool] -> a
deserialize = fromUniversal . fromBinary

fromBinary :: [Bool] -> Universal
fromBinary = fst . parseBinary

parseBinary :: [Bool] -> (Universal, [Bool])
parseBinary (False:False:bs) = (Unit,bs)
parseBinary (False:True:bs) = (Pair u v, bs2)
 where (u, bs1) = parseBinary bs
       (v, bs2) = parseBinary bs1
parseBinary (True:False:bs) = (This u, bs')
 where (u, bs') = parseBinary bs
parseBinary (True:True:bs) = (That u, bs')
 where (u, bs') = parseBinary bs

{-
In the end we want to test the wanted property using QuickCheck.
-}

deserializeSerialized :: (Eq a, Generic a) => a -> Bool
deserializeSerialized x = deserialize (serialize x) == x

testDeserialization :: IO ()
testDeserialization = do
  quickCheck (deserializeSerialized :: Bool -> Bool)
  quickCheck (deserializeSerialized :: [Bool] -> Bool)
  quickCheck (deserializeSerialized :: Colour -> Bool)
  quickCheck (deserializeSerialized :: [[Colour]] -> Bool)

instance Arbitrary Colour where
  arbitrary = elements [Red, Green, Blue, Yellow]

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
