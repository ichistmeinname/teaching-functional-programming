module Observer (runO, o0, o1, o2, o3, o4, Observe(..), observe) where

import Data.IORef
import System.IO.Unsafe
import Control.Exception 

type EvalRef = IORef EvalTree

data EvalTree = Cons String [EvalRef]
              | Uneval
              | Demand
              | Fun [(EvalRef,EvalRef)]

showEvalRef :: EvalRef -> IO String
showEvalRef ref = readIORef ref >>= showEvalTree
 
showEvalTree :: EvalTree -> IO String
showEvalTree Uneval = return "_"
showEvalTree Demand = return "!"
showEvalTree (Cons consName argRefs) = do
  args <- mapM readIORef argRefs
  argStrs <- mapM showEvalTree args
  return $ "(" ++ consName ++ " " ++ unwords argStrs ++ ")" 
showEvalTree (Fun appls) = do
  resStrs <- mapM showApp (reverse appls)
  return $ unlines resStrs
    where 
      showApp (rArg,rRes) = do
        arg <- showEvalRef rArg
        res <- showEvalRef rRes
        return $ concat ["{",arg,"->",res,"}"]

testEvalTree = do
  uneval <- newIORef Uneval
  one <- newIORef (Cons "1" [])
  n1 <- newIORef (Cons "(:)" [one,uneval])
  n <- newIORef (Cons "(:)" [uneval,n1])
  str <- showEvalRef n
  putStrLn str


data Tree = Empty | Node Tree Tree

oTree :: Tree -> EvalRef -> Tree

oTree Empty = o0 Empty "Empty"

oTree (Node tl tr) = o2 Node "Node" tl tr

o0 :: a -> String -> EvalRef -> a  
o0 cons consName ref = unsafePerformIO $ do
  mkEvalTreeCons consName ref 0
  return cons

o1 :: Observe a => (a -> b) -> String -> a -> EvalRef -> b
o1 cons consName vA ref = unsafePerformIO $ do
  [aRef] <- mkEvalTreeCons consName ref 1
  return $ cons (observer vA aRef)

o2 :: (Observe a, Observe b) => 
      (a -> b -> c) -> String -> a -> b -> EvalRef -> c
o2 cons consName vA vB ref = unsafePerformIO $ do
  [refl,refr] <- mkEvalTreeCons consName ref 2
  return (cons (observer vA refl) (observer vB refr))

o3 :: (Observe a, Observe b, Observe c)
   => (a -> b -> c -> d) -> String -> a -> b -> c -> EvalRef -> d
o3 cons consName vA vB vC ref = unsafePerformIO $ do
  [aRef, bRef, cRef] <- mkEvalTreeCons consName ref 3
  return $ cons (observer vA aRef) (observer vB bRef) (observer vC cRef)

o4 :: (Observe a, Observe b, Observe c, Observe d)
   => (a -> b -> c -> d -> e) -> String -> a -> b -> c -> d -> EvalRef -> e
o4 cons consName vA vB vC vD ref = unsafePerformIO $ do
  [aRef, bRef, cRef, dRef] <- mkEvalTreeCons consName ref 4
  return $ cons (observer vA aRef) (observer vB bRef)
                (observer vC cRef) (observer vD dRef)

mkEvalTreeCons :: String -> EvalRef -> Int -> IO [EvalRef]
mkEvalTreeCons consName ref arity = do
  refs <- sequence $ replicate arity $ newIORef Uneval
  writeIORef ref (Cons consName refs)
  return refs

isNode (Node _ _) = True
isNode _          = False

class Observe a where

   obs :: a -> EvalRef -> a

instance Observe Tree where

  obs = oTree

instance Observe a => Observe [a] where

  obs []     = o0 [] "[]"
  obs (x:xs) = o2 (:) "(:)" x xs

instance Observe Int where

  obs n = seq n $ o0 n (show n)
  -- Without seq n the number will be evaluated, when the show is needed.
  -- In case of an inifinite loop, this will be in the production of the
  -- observe output. seq here is similar to the pattern matching in the other
  -- cases.

instance (Observe a, Observe b) => Observe (a -> b) where
  obs f ref x = unsafePerformIO $ do
    applRefs <- readIOFunRef
    argRef <- newIORef Uneval
    resRef <- newIORef Uneval
    writeIORef ref $ Fun ((argRef,resRef) : applRefs) 
    return $ observer (f (observer x argRef)) resRef
     where 
       readIOFunRef = do
           v <- readIORef ref
           case v of
             Fun applRefs -> return applRefs
             _            -> do
                 writeIORef ref (Fun [])
                 return []

observer :: Observe a => a -> EvalRef -> a
observer x ref = unsafePerformIO $ do
  writeIORef ref Demand
  return $ obs x ref

global :: IORef [IO ()]
global = unsafePerformIO $ newIORef []

observe :: Observe a => String -> a -> a
observe label x = unsafePerformIO $ do
  ref <- newIORef Uneval
  modifyIORef global (showInfo ref :)
  return (observer x ref)
    where
      showInfo ref = do
         putStrLn (label ++ "\n" ++ replicate (length label) '-')
         showEvalRef ref >>= putStrLn


runO :: IO a -> IO ()
runO act = do
  catch (act>>return ()) (\(SomeException _) -> return ())
  observations <- readIORef global
  putStrLn ">>> Observations <<<"
  putStrLn "--------------------"
  sequence observations
  return ()

main2 = runO $ do
  let tree = observe "tree" $ Node (if 42 `div` 0 == 73 then Empty else Empty ) Empty
  print $ isNode tree
  let Node tl tr = tree
  print $ isNode tl
  

main = runO $ do
  let l = observe "list" $ repeat (42 ::Int)
  let fun = observe "take" take
  print (l !! 10)
  print (fun 5 l)
  print (length $ fun 2 [1::Int,2,3])

