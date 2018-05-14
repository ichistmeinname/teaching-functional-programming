(Finite) automata can be seen as (the unfolding) of infinite trees.
You can define the following data type in Haskell using this idea.

> data Automaton a = State Bool [(a, Automaton a)]

The Boolean value indicates if the current state is a final state. The list containt all possible successor states. Here, you can use the fact that the atuomaton is supposed to be deterministic, that is, each value of type `a` has exactly on corresponding entry in the list of successors.

1. Implement an automaton that accepts words of the language $a^*b^*$.

> aStarbStar :: Automaton Char
> aStarbStar = undefined

2. Implement a Haskell function that checks if the automaton accepts a given word.

> checkWord :: Eq a => Automaton a -> [a] -> Bool
> checkWord = undefined

3. Transfer your implementation to Elm (or a strict language of your choice) using the ideas discussed in class to simulate non-strictness in strict languages. It is not necessary to use non-strictness for all components, so try to use non-strictness only where it's really needed.

