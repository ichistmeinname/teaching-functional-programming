module Automaton exposing (checkWord, aStarbStar)

{-
 3. Transfer your implementation to Elm (or a strict language of your choice) using the ideas discussed in class to simulate non-strictness in strict languages. It is not necessary to use non-strictness for all components, so try to use non-strictness only where it's really needed.
-}

-- Elm already provides functionality for non-strict components
import Lazy exposing (Lazy, lazy, force)

import Tuple exposing (first, second)
import List.Extra exposing (find)

{-  The only component we need to evaluate in a non-strict manner is
    the successor automaton.
-}
type Automaton a = State Bool (List (a, Lazy (Automaton a)))

isFinal : Automaton a -> Bool
isFinal aut = case aut of
                  State final _ -> final

successors : Automaton a -> (List (a, Lazy (Automaton a)))
successors aut = case aut of
                     State _ lxs -> lxs

checkWord : Automaton a -> List a -> Bool
checkWord aut xs = case xs of
                       [] -> isFinal aut
                       y::ys -> case find (((==) y) << first) (successors aut) of
                                    Nothing -> False
                                    Just (_,newAut) -> checkWord (force newAut) ys

aStarbStar : Automaton Char
aStarbStar = State True [('a',lazy (\_ -> aStarbStar)),('b', lazy (\_ -> bStar))]

bStar : Automaton Char
bStar = State True [('b',lazy (\_ -> bStar))]

{-

> Automaton.checkWord Automaton.aStarbStar ['a','a','a','c','b','b','b']
False : Bool
> Automaton.checkWord Automaton.aStarbStar ['a','a','a','a','b','b','b']
True : Bool

-}
