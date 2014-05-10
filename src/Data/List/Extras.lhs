> module Data.List.Extras (pairs, argmax) where
>
> import Control.Applicative ((<*>))
> import Data.Function (on)
> import Data.List (maximumBy)

`pairs` takes a list, and returns a list of consecutive elements pairs:

> pairs :: [a] -> [(a,a)]
> pairs = zip <*> tail

`argmax` takes a function and a list, and returns the element of the
list for which the given function attains its maximum value:

> argmax :: Ord b => (a -> b) -> [a] -> a
> argmax = maximumBy . on compare
