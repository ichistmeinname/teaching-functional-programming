(Finite) automata can be seen as (the unfolding) of infinite trees.
You can define the following data type in Haskell using this idea.

> data Automaton a = State Bool [(a, Automaton a)]

> data Automaton2 a = State Bool (a -> Automaton a)
> data Automaton3 a = State Bool [(a, () -> Automaton a)]
> 
The Boolean value indicates if the current state is a final state. The list containt all possible successor states. Here, you can use the fact that the automaton is supposed to be deterministic, that is, each value of type `a` has exactly on corresponding entry in the list of successors.

1. Implement an automaton that accepts words of the regular expression a*b*.

> aStarbStar :: Automaton Char
> aStarbStar = State True [('a',aStarbStar),('b',bStar)]
>
> bStar :: Automaton Char
> bStar = State True [('b',bStar)]

2. Implement a Haskell function that checks if the automaton accepts a given word.

> checkWord :: Eq a => Automaton a -> [a] -> Bool
> checkWord (State final _) []     = final
> checkWord (State _ ts)    (x:xs) =
>     maybe False (flip checkWord xs) (lookup x ts)
>  --   case lookup x ts of
>  --     Nothing -> False
>  --     Just next -> checkWord next xs

   
   maybe :: b -> (a -> b) -> Maybe a -> b

   data Maybe a = Just a | Nothing
   
   foldMaybe :: (a -> b) -> b -> Maybe a -> b
   foldMaybe just nothing (Nothing) = nothing
   foldMaybe just nothing (Just x)  = just x

   foldList :: (a -> b -> b) -> b -> List a -> b
   foldList cons nil Nil = nil
   foldList cons nil (Cons x xs) = cons x (foldList cons nil xs)

  data List a = Cons a (List a) | Nil
   

   fmap :: (a -> b) -> Maybe a -> Maybe b
   (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
lookup :: Eq a => [(a,b)] -> a -> Maybe b

3. Transfer your implementation to Elm (or a strict language of your choice) using the ideas discussed in class to simulate non-strictness in strict languages. It is not necessary to use non-strictness for all components, so try to use non-strictness only where it's really needed.
