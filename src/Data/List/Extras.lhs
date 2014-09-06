> module Data.List.Extras (pairs, argmax, argsum) where
>
> import Control.Applicative ((<*>))
> import Data.Functor.Infix ((<$$>))
> import Data.Function (on)
> import Data.List (maximumBy)

`pairs` takes a list, and returns a list of consecutive elements pairs:

> pairs :: [a] -> [(a,a)]
> pairs = zip <*> tail

`argmax` takes a function and a list, and returns the element of the
list for which the given function attains its maximum value:

> argmax :: Ord b => (a -> b) -> [a] -> a
> argmax = maximumBy . on compare

`argsum` takes a function and a list, and returns the sum of the elements
produced by applying the function to each member of the list:

> argsum :: Num b => (a -> b) -> [a] -> b
> argsum = sum <$$> map
