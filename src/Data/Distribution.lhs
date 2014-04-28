> module Data.Distribution (Distribution(..), choose, (<~~), chooseMany) where
>
> import Control.Monad.Random (evalRand, fromList)
> import System.Random (RandomGen(..))

Distribution represents a discrete probability distribution. More
specifically: given some type parameter, a Distribution relates each
value of that type with the probability of its occurrence.

> type Distribution a = [(a, Rational)]

`choose` takes a random seed and a Distribution, and produces a value
according to that Distribution.

> choose :: RandomGen g => g -> Distribution a -> a
> choose seed = (`evalRand` seed) . fromList
>
> (<~~) :: RandomGen g => Distribution a -> g -> a
> (<~~) = flip choose

`chooseMany` takes a random seed and a Distribution, and produces an
infinite list of values according to that Distribution. Exercise: why
shouldn't this be implemented as `map choose . repeat . fromList`?

> chooseMany :: RandomGen g => g -> Distribution a -> [a]
> chooseMany seed = (`evalRand` seed) . sequence . repeat . fromList

