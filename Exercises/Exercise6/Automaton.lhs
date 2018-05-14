(Finite) automata can be seen as (the unfolding) of infinite trees.
You can define the following data type in Haskell using this idea.

> data Automaton a = State Bool [(a, Automaton a)]

The Boolean value indicates if the current state is a final state. The list containt all possible successor states. Here, you can use the fact that the atuomaton is supposed to be deterministic, that is, each value of type `a` has exactly on corresponding entry in the list of successors.

1. Implement an automaton that accepts words of the language $a^*b^*$.

Our first observation is that we only need two states to model this language.
If we read an $a$, we stay in the first state, if we read an $b$, we turn to an automaton that only accepts $b^*$.
   
> aStarbStar :: Automaton Char
> aStarbStar = State True [('a',aStarbStar),('b',bStar)]
>
> bStar :: Automaton Char
> bStar = State True [('b',bStar)]

2. Implement a Haskell function that checks if the automaton accepts a given word.

The above model allows for a simple implementation of `checkWord`.
If the word is empty, we only need to check if we already are in a final state.
Otherwise we need to lookup the first character in the list of successor states.
If we find an entry, we check the word with respect to the found successor state.
Otherwise we directly yield `False`.
   
> checkWord :: Eq a => Automaton a -> [a] -> Bool
> checkWord (State final _) [] = final
> checkWord (State _ succs) (x:xs) = maybe False (flip checkWord xs) (lookup x succs)

3. Transfer your implementation to Elm (or a strict language of your choice) using the ideas discussed in class to simulate non-strictness in strict languages. It is not necessary to use non-strictness for all components, so try to use non-strictness only where it's really needed.
