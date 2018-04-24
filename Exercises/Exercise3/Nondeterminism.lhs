> import Control.Monad.Plus (MonadPlus (..))

We introduced the type class `MonadPlus` in class and discussed its connection to non-determinism.
   
Define a function that computes all permutations of a given list non-deterministically.

> permutations :: MonadPlus mp => [a] -> mp [a]
> permutations = undefined

On top of that, you can implement a function `solve`, such that `solve num s` non-deterministically computes a list of of `n` natural numbers that sum up to `s`.

> solve :: MonadPlus mp => Int -> Int -> mp [Int]
> solve = undefined
