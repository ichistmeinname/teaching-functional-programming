module Array where

data Array a = Branch (Array a) a (Array a)

emptyArray :: Array a
emptyArray = Branch emptyArray
                    (error "access to non-initialized element")
                    emptyArray

(!) :: Array a -> Int -> a
(Branch _ value _)    ! 0 = value
(Branch left _ right) ! n | even n    = right ! (n `div` 2 - 1)
                          | otherwise = left  ! (n `div` 2)

update :: Array a -> Int -> (a -> a) -> Array a
update (Branch left value right) 0 f = Branch left (f value) right
update (Branch left value right) n f
  | even n    = Branch left value (update right (n `div` 2 - 1) f)
  | otherwise = Branch (update left (n `div` 2) f) value right

listToArray :: [a] -> Array a
listToArray xs = foldr (\(index,value) arr -> update arr index (const value)) emptyArray (zip [0..] xs)
