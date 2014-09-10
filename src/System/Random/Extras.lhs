> module System.Random.Extras (split3) where
>
> import System.Random (RandomGen(..))

Obtain a vector of three distinct random generators from one. TODO:
learn implementation's randomness properties.

> split3 :: RandomGen g => g -> (g, g, g)
> split3 seed = (b, c, d)
>   where (a, b) = split seed
>         (c, d) = split a
