> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections       #-}
>
> module Data.Distribution (
>   Distribution,
>   Probability,
>   probability,
>   (<?),
>   (?>),
>   choose,
>   (<~~),
>   chooseMany,
>   uniform
> ) where
>
> import Prelude hiding (lookup, map)
> import Data.Maybe (fromMaybe)
> import Data.Function (on)
> import Data.Functor.Infix ((<$$>))
> import Data.Ratio ((%))
> import Data.Set (Set, size, map, toList)
> import Data.Set.Extras (lookup)
> import Control.Monad.Random (MonadRandom, evalRand, fromList)
> import System.Random (RandomGen(..))

We represent probabilities as ratios of `Integer`s , with no strict
bounds-checking:

> type Probability = Rational

Distribution represents a discrete probability distribution. More
specifically: given some type parameter, a Distribution relates each
value of that type with the probability of its occurrence.

> type Distribution a = Set (a, Probability)

Analogous to `fromList` in `Control.Monad.Random`, `fromSet` samples a
random value from a distribution, where the total weight of all events
must not be zero. Should fuse.

> fromSet :: Ord a => MonadRandom m => Distribution a -> m a
> fromSet = fromList . toList -- "The *special* hell." (Should fuse.)

`choose` takes a random seed and a Distribution, and produces a value
according to that Distribution.

> choose :: Ord a => RandomGen g => g -> Distribution a -> a
> choose seed = (`evalRand` seed) . fromSet
>
> (<~~) :: Ord a => RandomGen g => Distribution a -> g -> a
> (<~~) = flip choose

`chooseMany` takes a random seed and a Distribution, and produces an
infinite list of values according to that Distribution. Exercise: why
shouldn't this be implemented as `map choose . repeat . fromSet`?

> chooseMany :: Ord a => RandomGen g => g -> Distribution a -> [a]
> chooseMany seed = (`evalRand` seed) . sequence . repeat . fromSet

`probability` takes a Distribution and a value, and returns the
likelihood of that value occurring per that distribution. Should a
probability not be defined for that value, this likelihood is zero:

> probability :: Eq a => Ord a => a -> Distribution a -> Probability
> probability = fromMaybe (0%1) <$$> lookup
>
> (<?) :: Eq a => Ord a => a -> Distribution a -> Probability
> (<?) = probability
>
> (?>) :: Eq a => Ord a => Distribution a -> a -> Probability
> (?>) = flip probability

`uniform` constructs a uniform discrete distribution over a given set:

> uniform :: Ord a => Set a -> Distribution a
> uniform = map =<< flip (,) . (1//) . size
>   where (//) = (/) `on` toRational
