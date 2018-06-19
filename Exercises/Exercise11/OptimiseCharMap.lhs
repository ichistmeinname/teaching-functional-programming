In the lecture, we implemented CharMap as a list of tuples.
Optimise this data structure using the modules Data.Map or Data.IntMap.
Compare the execution time for operations on large trie structures. Give some meaningful benchmarks.

> import Prelude hiding (filter, lookup)
> import Data.Map
> import Data.IntMap
>

Choose whatever import you like best.

> type CharMap a = Map Char a
> -- type CharMap a = IntMap a
> 
> emptyCharMap :: CharMap a
> emptyCharMap = undefined
> 
> lookupChar :: Char -> CharMap a -> Maybe a
> lookupChar = undefined
> 
> insertChar :: Char -> a -> CharMap a -> CharMap a
> insertChar = undefined
> 
> deleteChar :: Char -> CharMap a -> CharMap a
> deleteChar = undefined
> 
> updateChar :: Char -> (Maybe a -> Maybe a) -> CharMap a -> CharMap a
> updateChar = undefined
