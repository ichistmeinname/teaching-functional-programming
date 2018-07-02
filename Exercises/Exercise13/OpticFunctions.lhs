Up to now we only discussed accessors for data types that are sum or product types in a quite obvious way.
In this exercise we like to take a look at some data types and functions on them that are sum and product types in disguise!
Define a lens/prism for the following functions and argue what's suited best (if any is preferable)!
The original functionality should then be accessed using `view` or `preview`.

For example, for `headL` we want to have the following behaviour.

   ```{.haskell}
   > view headL [1..10]
   1
   ```


(1) Define `head` based on lenses/prisms.

> headL = undefined
> headP = undefined

(2) Define `(!!)` based on lenses/prisms.

> idxL idx = undefined
> idxP idx = undefined

(3) Define `member` based on lenses/prisms.

> -- behaves like `member` on `Data.Set`
> containsL val = undefined
> containsP val = undefined

(4) Define `lookup` based on lenses/prisms.

> -- behaves like `lookup`
> lookupL val = undefined
> lookupP val = undefined
