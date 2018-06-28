> import Tries

Define a trie data type `BinMap` with corresponding functions `empty`, `lookup` and `update` for keys of type `Bin`.

> data Bin2 = Tip | Bin2 Bin2 String Bin2

> emptyBin = undefined
> lookupBin = undefined
> updateBin = undefined

Implement a function

> stringMapToList :: StringMap a -> [(String,a)]
> stringMapToList = undefined

which converts a `StringMap` into a list of the stored key-value pairs.

Implement a monoid instance for the type `TreeMap a`, where `mplus` combines two `TreeMap` structures.
Try to implemet this function efficiently.
If both `TreeMap`s contain an entry, the value of the first `TreeMap` should be prefered.
