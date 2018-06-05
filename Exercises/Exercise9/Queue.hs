type Que a = [a]

emptyQue = []

enque :: a -> Que a -> Que a
enque x q = q ++ [x]

deque :: Que a -> Que a
deque = tail

peekQue :: Que a -> a
peekQue = head

{-
enque 42
  (enque 43
     (enque 44
        emptyQue))
=> (([] ++ [44]) ++ [43]) ++ [42]
For n enqueue operations we obtain runtime of O(n²)
-}

data Queue a = Q [a] [a]

emptyQueue = Q [] []

isEmptyQueue :: Queue a -> Bool
isEmptyQueue (Q xs ys) = null xs

enqueue x (Q xs ys) = queue xs (x:ys)

peek (Q (x:_) _) = x

dequeue (Q (_:xs) ys) = queue xs ys

-- Invariant: If the first list is empty, then also the second list is empty.

queue :: [a] -> [a] -> Queue a
queue [] ys = Q (reverse ys) []
queue xs ys = Q xs ys

{-
(dequeue              -- n-1 steps with constant time
  (dequeue
    (dequeue          -- 1 step with linear time in number of enques from before
      (enqueue 42     -- n steps with constant time
        (enqueue 43
          (enqueue 44 -- reverse for list of length 1
             emptyQueue))...)
=> 
For n enqueue/dequeue operations we obtain runtime of O(n)
-}
