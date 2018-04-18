EDIT: If you care for different solutions to compute the diagonals of a matrix, you can check out
      the following StackOverflow discussion.

      https://stackoverflow.com/questions/32465776/getting-all-the-diagonals-of-a-matrix-in-haskell

Let's take a look at some snippets of your code and refactor it!

First, we declare some imports (where we explicitly name the functions that we need!)
and define a data type to represent the board as well as an example board.

> import Data.List (transpose, subsequences, group)
> import Control.Monad (msum)
> 
> data Board1 = Board1 [[Int]]
> 
> test :: Board1
> test = Board1 [[0,1,0,0,0,0]
>               ,[1,0,2,0,0,0]
>               ,[1,0,1,2,0,0]
>               ,[1,1,1,0,2,0]
>               ,[0,1,1,0,0,0]
>               ,[1,1,1,0,0,0]]

> checkWin :: Board1 -> Bool
> checkWin (Board1 rows) = checkLines rows
>                       || checkLines ((reverse . transpose) rows)
>                       || checkLines (diagonals rows ++ diagonals ((reverse . transpose) rows))

First up, we realise that we have multiply arguments that we want to combine using |(||)|.
Fortunately, the Prelude already defines a nice convenient function to folds a list of booleans
with |(||)|, it's named |or|. We can rewrite the above code using |or| as follows.

> checkWinR1 :: Board1 -> Bool
> checkWinR1 (Board1 rows) = or [ checkLines rows
>                               , checkLines ((reverse . transpose) rows)
>                               , checkLines (diagonals rows ++ diagonals ((reverse . transpose) rows))
>                               ]

Secondly, we observe that we actually apply the same function, |checkLines|, in every argument of the list.
Using |map|, we can refactor the code a second time.

> checkWinR2 :: Board1 -> Bool
> checkWinR2 (Board1 rows) = or (map checkLines
>                                    [ rows
>                                    , (reverse.transpose) rows
>                                    , diagonals rows ++ diagonals ((reverse.transpose) rows)
>                                    ])

The next thing we can observe, which we actually did not discuss during our meeting, is that
the argument |row| is used in every list argument as well. In order to see it better, we can
redefine the third condition using an anonymous function.

> checkWinR3 :: Board1 -> Bool
> checkWinR3 (Board1 rows) = or (map checkLines
>                                    [ rows
>                                    , (reverse.transpose) rows
>                                    , (\r -> diagonals r ++ diagonals ((reverse.transpose) r)) rows
>                                    ])

Now we can refactor the code and get rid of the rows argument in every element of the list.

> checkWinR4 :: Board1 -> Bool
> checkWinR4 (Board1 rows) = or (map (\ f -> checkLines (f rows))
>                                    [ id
>                                    , reverse . transpose
>                                    , (\r -> diagonals r ++ diagonals ((reverse.transpose) r))
>                                    ])

At the very end, we can make the function pointfree.

> checkWinR5 :: Board1 -> Bool
> checkWinR5 (Board1 rows) = or (map (checkLines . ($ rows))
>                                    [ id
>                                    , reverse . transpose
>                                    , (\r -> diagonals r ++ diagonals ((reverse.transpose) r))
>                                    ])

You can just choose the solution you like the most ; )
The point is that we can go from a solid solution and refactor it to become more compact in
several individual steps. Sometimes the possibilites seems endless*, so you need to keep in mind that
sometimes enough is enough!

In order to make the above code compile, we need the following helper functions.

> checkLines []     = False
> checkLines (l:ls) = checkList l 0 (length l) || checkLines ls
> 
> checkList r i mi = if i > mi - 4 then False
>                                  else (r!!i /= 0 && r!!i == r!!(i+1) && r!!i == r!!(i+2) && r!!i == r!!(i+3)) || checkList r (i+1) mi
> diagonals []       = []
> diagonals ([]:xss) = xss
> diagonals xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
>                                       ([]:(diagonals (map tail xss)))
> 

Another solution has a similar flavour, that we can also refactor.

> winningMove :: Board1 -> Int -> Int -> Int -> Bool
> winningMove (Board1 xs) player i j | isWinning (extractColumn xs i) player
>                                   || isWinning (extractRow xs j) player
>                                   || isWinning (extractDiagUp xs i j) player
>                                   || isWinning (extractDiagDown xs i j) player = True
>                                   | otherwise                                  = False

First, we do not need to distinguish two cases if we yield boolean anyway.

> winningMoveR1 :: Board1 -> Int -> Int -> Int -> Bool
> winningMoveR1 (Board1 xs) player i j = isWinning (extractColumn xs i) player
>                                     || isWinning (extractRow xs j) player
>                                     || isWinning (extractDiagUp xs i j) player
>                                     || isWinning (extractDiagDown xs i j) player

Second, we use |or|.
        
> winningMoveR2 :: Board1 -> Int -> Int -> Int -> Bool
> winningMoveR2 (Board1 xs) player i j = or [ isWinning (extractColumn xs i) player
>                                           , isWinning (extractRow xs j) player
>                                           , isWinning (extractDiagUp xs i j) player
>                                           , isWinning (extractDiagDown xs i j) player
>                                           ]

Third, we use |map| again.

> winningMoveR3 :: Board1 -> Int -> Int -> Int -> Bool
> winningMoveR3 (Board1 xs) player i j = or (map (\extracted -> isWinning extracted player)
>                                                [ extractRow xs j
>                                                , extractDiagUp xs i j
>                                                , extractDiagDown xs i j
>                                                ])

Forth, we unify the functions in order to pass |xs|, |i| and |j| to all the functions.

> winningMoveR4 :: Board1 -> Int -> Int -> Int -> Bool
> winningMoveR4 (Board1 xs) player i j = or (map (\extracted -> isWinning extracted player)
>                                                [ extractRow' xs i j
>                                                , extractDiagUp xs i j
>                                                , extractDiagDown xs i j
>                                                ])

Fifth, we only list the functions and supply the arguments in the function we provide for |map|.

> winningMoveR5 :: Board1 -> Int -> Int -> Int -> Bool
> winningMoveR5 (Board1 xs) player i j = or (map (\extracted -> isWinning (extracted xs i j) player)
>                                                [ extractRow'
>                                                , extractDiagUp
>                                                , extractDiagDown
>                                                ])

And in the end, we can make the anonymous function pointfree again.

> winningMoveR6 :: Board1 -> Int -> Int -> Int -> Bool
> winningMoveR6 (Board1 xs) player i j = or (map (flip isWinning player . ($ j) . ($ i) . ($ xs))
>                                                [ extractRow'
>                                                , extractDiagUp
>                                                , extractDiagDown
>                                                ])

Since it does not look very intuitive to supply three partial applications of |($)|, we use a local definition
for a version of |($)| that takes three arguments.

> winningMoveR7 :: Board1 -> Int -> Int -> Int -> Bool
> winningMoveR7 (Board1 xs) player i j = or (map (flip isWinning player . (($$$) xs i j))
>                                                [ extractRow'
>                                                , extractDiagUp
>                                                , extractDiagDown
>                                                ])
>  where
>   ($$$) :: a -> b -> c -> (a -> b -> c -> d) -> d
>   ($$$) x y z f = f x y z

Since the definitions are rather complex in this case, we only define dummy definitions to make
the code compile, but not executable.

> extractRow' :: [[Int]] -> Int -> Int -> Int -> [Int]
> extractRow' xs i j = extractRow xs j
> isWinning = undefined
> extractColumn = undefined
> extractRow = undefined
> extractDiagUp = undefined
> extractDiagDown = undefined
