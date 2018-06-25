> import qualified Tries as T

Enhance the implementation for the function `deleteString` discussed in the lecture.
Currently, the implementation yields subtrees without entries when deleting entries.
Let's take a look at the following examples.

      ghci> insertString "ab" 42 $ insertString "a" 43 emptyStringMap
      StringMap Nothing [('a',StringMap (Just 43) [('b',StringMap (Just 42) [])])]
      ghci> deleteString "ab" it
      StringMap Nothing [('a',StringMap (Just 43) [('b',StringMap Nothing [])])]

The result of the second example resembles the expression

    StringMap Nothing [('a',StringMap (Just 43) [])]

but keeps an empty entry with key "ab".

Implement a variant of `deleteString`, such that these empty entries do not occur.
Hence, the above example should yield the "smaller" trie as result.

> deleteString :: String -> T.StringMap a -> T.StringMap a
> deleteString []     (T.StringMap _ b) = T.StringMap Nothing b
> deleteString (c:cs) (T.StringMap a b) =
>   T.StringMap a (maybe b (\d -> let m = T.deleteString cs d
>                                 in if isEmptyStringMap m
>                                    then T.deleteChar c b
>                                    else T.insertChar c m b)
>                        (T.lookupChar c b))
> 
> 
> isEmptyStringMap :: T.StringMap a -> Bool
> isEmptyStringMap (T.StringMap Nothing m) = all (isEmptyStringMap . snd) m
> isEmptyStringMap _                     = False
> 
> updateString :: String -> (Maybe a -> Maybe a) -> T.StringMap a -> T.StringMap a
> updateString []     upd (T.StringMap a b) = T.StringMap (upd a) b
> updateString (c:cs) upd (T.StringMap a b) =
>   T.StringMap a (T.updateChar c
>                 (prune . updateString cs upd . maybe T.emptyStringMap id)
>                 b)
>  where
>   prune m = if isEmptyStringMap m then Nothing else Just m
