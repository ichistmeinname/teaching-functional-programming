Extend your dequeue implementation from Execise 9 to an indexed dequeue,
which also allows indexed access to each dequeue element.
Implement the following interface.

> data IdxDequeue a
> 
> empty :: IdxDequeue a
> empty = undefined
>
> null :: IdxDequeue a -> Bool
> null = undefined
>
> length :: IdxDequeue a -> Int
> length = undefined
>
> head :: IdxDequeue a -> a
> head = undefined
>
> last :: IdxDequeue a -> a
> last = undefined
>
> (<.) :: a -> IdxDequeue a -> IdxDequeue a
> (<.) = undefined
>
> (.>) :: IdxDequeue a -> a -> IdxDequeue a
> (.>) = undefined
>
> tail :: IdxDequeue a -> IdxDequeue a
> tail = undefined
>
> init :: IdxDequeue a -> IdxDequeue a
> init = undefined
>
> reverse :: IdxDequeue a -> IdxDequeue a
> reverse = undefined
>
> replicate :: Int -> a -> IdxDequeue a
> replicate = undefined
>
> fromList :: [a] -> IdxDequeue a
> fromList = undefined
>
> toList :: IdxDequeue a -> [a]
> toList = undefined
>
> (!) :: IdxDequeue a -> Int -> a
> (!) = undefined
>
> modify :: Int -> (a -> a) -> IdxDequeue a -> IdxDequeue a
> modify = undefined


For the data type and the implementation you should start with the list-based
 version of the Dequeue and replace the lists by array lists.

To obtain an efficient implementation, it might be useful to to store some addtional information.
Argue about the runtime complexity of each function.
