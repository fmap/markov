> module Data.List.Extras (pairs, argsum) where
>
> import Control.Applicative ((<*>))
> import Data.Functor.Infix ((<$$>))

`pairs` takes a list, and returns a list of consecutive elements pairs:

> pairs :: [a] -> [(a,a)]
> pairs = zip <*> tail

`argsum` takes a function and a list, and returns the sum of the elements
produced by applying the function to each member of the list:

> argsum :: Num b => (a -> b) -> [a] -> b
> argsum = sum <$$> map
