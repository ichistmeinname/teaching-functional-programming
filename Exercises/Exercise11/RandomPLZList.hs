module RandomPLZList where

import Control.Monad (replicateM)
import System.Random (randomRIO)

randomFile :: IO ()
randomFile = do
  let rand = do 
        c <- randomRIO('A','Z')
        w <- replicateM 20 $ randomRIO('a','z')
        n <- randomRIO(0,99999 :: Int)
        return (c:w, n)
  content <- replicateM 100000 rand
  writeFile "RandomPLZList.hs" $
    unlines ["module RandomPLZList where",
             "",
             "plzList :: [(String, Int)]",
             "plzList = " ++ show content]
