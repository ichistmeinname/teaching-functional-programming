> import Parser

Implement a parser for the following XML data type.

> data XData
>   -- |Normaler Text
>   = XText String
>   -- |Knoten mit Attributen und Kindknoten
>   | XElem String [Attr] [XData]
>   deriving Show
> 
> infix 7 :=
> 
> data Attr = String := String
>   deriving Show

Define the corresponding Read instance and test your implementation with some examples.

> instance Read XData wher
>     readsPrec _ = undefined

> test :: String
> test = "<person first=\"Frank\" last=\"Huch\"><email>fhu@informatik.uni-kiel.de</email></person>"
