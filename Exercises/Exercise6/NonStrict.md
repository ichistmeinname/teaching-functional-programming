In class we defined non-strict lists in a strict language using Elm.

Now it's your turn to chose a strict language to implement a non-strict version of lists using the same idea.

#. Implement at least the functions `nil`, `isNil`, 
    `head`, `tail`, `append` und `from` as we did in class.
    Additionally, you shoul define a function `toList` that converts a non-strict list into an ordinary list.

#.  (In)finite lists, which obeys a specific law, can be simply defined using that law. Giv
    Give a valid implementation that can generate a non-strict list using `from 0`.

#.  Implement the Sieve of Eratosthenes using your non-strict implementation and infinite lists.
