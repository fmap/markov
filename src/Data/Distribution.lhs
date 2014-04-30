> module Data.Distribution (Distribution(..), Probability, probability, (<?), (?>), choose, (<~~), chooseMany) where
>
> import Data.Maybe (fromMaybe)
> import Data.Functor.Extras ((<$$>))
> import Data.Ratio ((%))
> import Control.Monad.Random (evalRand, fromList)
> import System.Random (RandomGen(..))

We represent probabilities as ratios of `Integer`s , with no strict
bounds-checking:

> type Probability = Rational

Distribution represents a discrete probability distribution. More
specifically: given some type parameter, a Distribution relates each
value of that type with the probability of its occurrence.

> type Distribution a = [(a, Probability)]

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

`probability` takes a Distribution and a value, and returns the
likelihood of that value occurring per that distribution. Should a
probability not be defined for that value, this likelihood is zero:

> probability :: Eq a => a -> Distribution a -> Probability
> probability = fromMaybe (0%1) <$$> lookup 
>
> (<?) :: Eq a => a -> Distribution a -> Probability
> (<?) = probability
>
> (?>) :: Eq a => Distribution a -> a -> Probability
> (?>) = flip probability
