> import Parser
> import Control.Applicative (Alternative(..))
> import Data.Char (isAlpha, isSpace)

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

We define convenient parsers for letters and spaces.

> letter :: Parser Char
> letter = check isAlpha anyChar
> 
> space :: Parser Char
> space = check isSpace anyChar

An XML-expression is either a text or an element.

> xData = xElem <|> xText

We implement the actual parsers corresponding to the constructors in a monadic style.

> xElem = do (tag,attrs) <- xOpen
>            children <- many xData
>            word $ "</" ++ tag ++ ">"
>            return $ XElem tag attrs children
> 
> xOpen = do char '<'
>            tag <- some letter
>            attrs <- many xAttr
>            char '>'
>            return (tag,attrs)
> 
> xAttr = do some space
>            name <- some letter
>            word "=\""
>            value <- some (check (/='"') anyChar)
>            char '"'
>            return $ name := value

In order to keep it simple, text is everything not in angle brackets.

> xText = XText <$> some (check (`notElem`"<>") anyChar)

The Read instance only considers the first successful parsing result.
Otherwise we would have "ambiuous parses".
That is, we use the same behaviour underlying our implementation of `parse`.
The ambiguous parses orgin in the fact that our grammar allows to use several `XText` expression in sequence.
Due the definition of `many`, the only reasonable result is the one that is given first.
So in the end, we do not have to consider other results.

> instance Read XData where
>     readsPrec _ = take 1 . runParser xData

> test :: String
> test = "<person first=\"Frank\" last=\"Huch\"><email>fhu@informatik.uni-kiel.de</email></person>"

    λ> read "<person first=\"Frank\" last=\"Huch\"><email>fhu@informatik.uni-kiel.de</email></person>" :: XData
    XElem "person" ["first" := "Frank","last" := "Huch"] [XElem "email" [] [XText "fhu@informatik.uni-kiel.de"]]
