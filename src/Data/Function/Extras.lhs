> module Data.Function.Extras (fixed) where

`fixed f x` determines the fixed point of a function `f`, starting from
a point `x`:

> fixed :: Eq a => (a -> a) -> a -> a
> fixed f x = if x == x' then x else f x'
>   where x' = f x
