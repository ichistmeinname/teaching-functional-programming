> module Functions where
>
> import Data.List (intercalate)
> import Data.IORef
> import System.IO.Unsafe
> import Control.Exception 
>
> type EvalRef = IORef EvalTree
> 
> data EvalTree = Cons String [EvalRef]
>               | Uneval
>               | Demand
>               | Fun [(EvalRef,EvalRef)]
> 
> showEvalRef :: EvalRef -> IO String
> showEvalRef ref = readIORef ref >>= showEvalTree
>
> testEvalTree = do
>   uneval <- newIORef Uneval
>   one <- newIORef (Cons "1" [])
>   n1 <- newIORef (Cons "(:)" [one,uneval])
>   n <- newIORef (Cons "(:)" [uneval,n1])
>   str <- showEvalRef n
>   putStrLn str
> 
> 
> data Tree = Empty | Node Tree Tree
> 
> oTree :: Tree -> EvalRef -> Tree
> 
> oTree Empty = o0 Empty "Empty"
> 
> oTree (Node tl tr) = o2 Node "Node" tl tr
> 
> o0 :: a -> String -> EvalRef -> a  
> o0 cons consName ref = unsafePerformIO $ do
>   mkEvalTreeCons consName ref 0
>   return cons
> 
> o2 :: (Observe a, Observe b) => 
>       (a -> b -> c) -> String -> a -> b -> EvalRef -> c
> o2 cons consName vA vB ref = unsafePerformIO $ do
>   [refl,refr] <- mkEvalTreeCons consName ref 2
>   return (cons (observer vA refl) (observer vB refr))
> 
> mkEvalTreeCons :: String -> EvalRef -> Int -> IO [EvalRef]
> mkEvalTreeCons consName ref arity = do
>   refs <- sequence $ replicate arity $ newIORef Uneval
>   writeIORef ref (Cons consName refs)
>   return refs
> 
> isNode (Node _ _) = True
> isNode _          = False
> 
> class Observe a where
> 
>    obs :: a -> EvalRef -> a
> 
> instance Observe Tree where
> 
>   obs = oTree
> 
> instance Observe a => Observe [a] where
> 
>   obs []     = o0 [] "[]"
>   obs (x:xs) = o2 (:) "(:)" x xs
> 
> instance Observe Int where
> 
>   obs n = seq n $ o0 n (show n)
>   -- Without seq n the number will be evaluated, when the show is needed.
>   -- In case of an inifinite loop, this will be in the production of the
>   -- observe output. seq here is similar to the pattern matching in the other
>   -- cases.
> 
> instance (Observe a, Observe b) => Observe (a -> b) where
>   obs f ref x = unsafePerformIO $ do
>     applRefs <- readIOFunRef
>     argRef <- newIORef Uneval
>     resRef <- newIORef Uneval
>     writeIORef ref $ Fun ((argRef,resRef) : applRefs) 
>     return $ observer (f (observer x argRef)) resRef
>      where 
>        readIOFunRef = do
>            v <- readIORef ref
>            case v of
>              Fun applRefs -> return applRefs
>              _            -> do
>                  writeIORef ref (Fun [])
>                  return []
> 
> observer :: Observe a => a -> EvalRef -> a
> observer x ref = unsafePerformIO $ do
>   writeIORef ref Demand
>   return $ obs x ref
> 
> global :: IORef [IO ()]
> global = unsafePerformIO $ newIORef []
> 
> observe :: Observe a => String -> a -> a
> observe label x = unsafePerformIO $ do
>   ref <- newIORef Uneval
>   modifyIORef global (showInfo ref :)
>   return (observer x ref)
>     where
>       showInfo ref = do
>          putStrLn (label ++ "\n" ++ replicate (length label) '-')
>          showEvalRef ref >>= putStrLn
> 
> 
> runO :: IO a -> IO ()
> runO act = do
>   catch (act>>return ()) (\(SomeException _) -> return ())
>   observations <- readIORef global
>   putStrLn ">>> Observations <<<"
>   putStrLn "--------------------"
>   sequence observations
>   return ()
> 
> main2 = runO $ do
>   let tree = observe "tree" $ Node (if 42 `div` 0 == 73 then Empty else Empty ) Empty
>   print $ isNode tree
>   let Node tl tr = tree
>   print $ isNode tl
>   
> 
> main = runO $ do
>   let l = observe "list" $ repeat (42 ::Int)
>   let fun = observe "take" take
>   print (l !! 10)
>   print (fun 5 l)
>   print (length $ fun 2 [1::Int,2,3])


> showEvalTree :: EvalTree -> IO String
> showEvalTree Uneval = return "_"
> showEvalTree Demand = return "!"
> showEvalTree (Cons consName argRefs) = do
>   args <- mapM readIORef argRefs
>   argStrs <- mapM showEvalTree args
>   return $ "(" ++ consName ++ " " ++ unwords argStrs ++ ")" 

We need to modify `showEvalTree`, more precisely, the case of `Fun apps`.
In order to reduce the overall usage of parentheses, the helper function
`showApp` should not introduce parentheses for individual observation.
Instead, we insert parentheses and commas at the end.

> showEvalTree (Fun apps) = do
>   resStrs <- mapM showApp $ reverse apps
>   return $ "{ " ++ intercalate "\n, " resStrs ++ "\n}"

The function `showApp` could just call `showEvalTree` with arguments and results.
However, if we want to observe a 2-ary function, the result is a 1-ary function,
so that `showEvalTree` will add parentheses again.
Thus, we need to a helper function `shoeEvalTree'` that does not introduce
these unecessary parentheses.

> showApp :: (EvalRef,EvalRef) -> IO String
> showApp (rArg, rRes) = do
>   arg <- readIORef rArg >>= showEvalTree' True
>   res <- readIORef rRes >>= showEvalTree' False
>   return $ arg ++ " -> " ++ res
>  where

In a nutshell, `showEvalTree'` behaves as `showEvalTree`, but does not
introduce parentheses for functions that are only called once.
However, when passing higher-order function, we want to introduce such parentheses again!
So we use an additional boolean flag to indicate situation that need parentheses.

>   -- like showEvalTree, but does not introduce line breaks
>   showEvalTree' isArg (Fun [app]) = braceIf isArg `fmap` showApp app
>    where braceIf p s = if p then '{' : s ++ "}" else s

If the result of a top-level function is a function, again, that was called several times
(for example, when passing a higher order function in recursive calls), then we definetly
need parentheses.
However, we do not need the line break!

>   showEvalTree' _ (Fun  apps) = do
>     resStrs <-  mapM showApp $ reverse apps
>     return $ "{" ++ intercalate ", " resStrs ++ "}"

For cases that are not functions, the function just behaves as `showEvalTree`.

>   showEvalTree' _ other = showEvalTree other

In the end, we give some test cases to observe the different situations.

(1) No application

    λ> runO $ print $ foldr (observe "(+)" (+)) 0 [1..0::Int]
    0
    >>> Observations <<<

(2) two applications

    λ> runO $ print $ foldr (observe "(+)" (+)) 0 [1..2::Int]
    3
    >>> Observations <<<
    (+)
    ---
    { 1 -> 2 -> 3
    , 2 -> 0 -> 2
    }

(3) two applications and one partial application
    
    λ> runO $ print $ map (observe "(+)" (+) 1) [1..2::Int]
    [2,3]
    >>> Observations <<<
    (+)
    ---
    { 1 -> {1 -> 2, 2 -> 3}
    }

(4) like (2) but with higher-order

    λ> runO $ print $ foldr (observe "id" id (observe "(+)" (+))) 0 [1..2::Int]
    3
    >>> Observations <<<
    (+)
    ---
    { 1 -> 2 -> 3
    , 2 -> 0 -> 2
    }
    id
    --
    { {1 -> 2 -> 3, 2 -> 0 -> 2} -> {1 -> 2 -> 3, 2 -> 0 -> 2}
    }

(5) like (3) but with higher-order

    λ> runO $ print $ map   (observe "id" id (observe "(+)" (+)) 1) [1..2::Int]
    [2,3]
    >>> Observations <<<
    (+)
    ---
    { 1 -> {1 -> 2, 2 -> 3}
    }
    id
    --
    { {1 -> {1 -> 2, 2 -> 3}} -> 1 -> {1 -> 2, 2 -> 3}
    }

(6) one application after one higher-order partical application

    λ> runO $ print $ map   (observe "id" id (observe "(+)" (+)) 1) [1..1::Int]
    [2]
    >>> Observations <<<
    (+)
    ---
    { 1 -> 1 -> 2
    }
    id
    --
    { {1 -> 1 -> 2} -> 1 -> 1 -> 2
    }
