> import GenericProgramming

In the lecture we developed generic implementations for the equality check and for a serialisation function.

 (1) In Haskell it is possible to automatically derive instances for the type class `Ord`.
 Try different data types and compare values by means of the function `compare` to analyse,
 how the algorithm used to generate the derived instances works.
 Then define an `Ord` instance for `Universal` and a generic function

> genericCmp :: Generic a => a -> a -> Ordering
> genericCmp = undefined

such that for all presented instances of `Generic` the following property holds.

    ~~~
    genericCmp x y = compare x y
    ~~~

Use `QuickCheck` to test this property.
    
Define a `Generic` instance for the data type `Int`.
Does your instance fulfil the property?
If not, explain, how `Int` values are compared using `genericCmp`.    

(2) Extend the class `Generic` by a function

    ~~~
    fromUniversal :: Universal -> a
    ~~~

for converting `Universal` values into their original data types and extend the `Generic` instances from the lecture.
Furthermore, define a generic function `deserialise` for reading generic data from bit sequences.
Your implementation should fulfil the following property.

    ~~~
    deserialize . serialize = id
    ~~~

Again test your code by means of QuickCheck.
