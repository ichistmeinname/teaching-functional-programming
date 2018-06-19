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
> deleteString = undefined
