> module Data.List.Extras (pairs) where
>
> import Control.Applicative ((<*>))

`pairs` takes a list, and returns a list of consecutive elements pairs:

> pairs :: [a] -> [(a,a)]
> pairs = zip <*> tail
