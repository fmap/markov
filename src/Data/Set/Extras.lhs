> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Data.Set.Extras (
>   (<~>),
>   sum,
>   product,
>   foldl1,
>   arbitrary,
>   findMaxBy,
>   argmax,
>   argsum,
>   lookup,
>   concat,
>   sequence,
>   lift2,
>   lift3
> ) where 
>
> import Prelude hiding (foldl, foldr, null, foldl1, sum, product, map, sequence, lookup, concat)
> import qualified Data.List as List (foldr, lookup)
> import Data.Set 
> import Data.Function (on)
> import Data.Functor.Infix ((<$$>))

Specialised infix map with the arguments flipped. These sets aren't functors
because of the Ord constraint on their map implementation:

> (<~>) :: Ord b => Set a -> (a -> b) -> Set b
> (<~>) = flip map
>
> infixr 0 <~>

Sum of a finite numeric set.

> sum :: Num a => Set a -> a
> sum = foldl (+) 0
  
Product of a finite numeric set.

> product :: Num a => Set a -> a
> product = foldl (*) 1

Variant of 'foldl' with no starting argument. Cannot be applied to the empty
set.

> foldl1 :: (a -> a -> a) -> Set a -> a
> foldl1 function set
>   | null set  = error "Don't apply Data.Set.Extras.foldl1 to empty sets!"
>   | otherwise = foldl function (arbitrary set) set

Extract the first element we meet in traversing the set. I've only found this
useful having determined the set is a singleton. Cannot be applied to the empty
set.

> arbitrary :: Set a -> a
> arbitrary = foldr (\a _ -> a) (error "Don't apply Data.Set.Extras.arbitrary to the empty set!")

Maximum over a user-supplied comparison function.

> findMaxBy :: (a -> a -> Ordering) -> Set a -> a
> findMaxBy comparison = foldl1 maxBy
>   where maxBy x y = case comparison x y of { GT -> x; _ -> y }

Determines the point over a given set for which the function
attains its maximum value.

> argmax :: Ord b => (a -> b) -> Set a -> a
> argmax = findMaxBy . on compare

Given a function and a set, determines the sum of the terms
produced by applying the function to each element of the set.

> argsum :: Ord b => Num b => (a -> b) -> Set a -> b
> argsum = sum <$$> map

Looks up a key in an association list. Should fuse.

> lookup :: Eq a => Ord a => Ord b => a -> Set (a, b) -> Maybe b
> lookup key = List.lookup key . toList

Flatten a set of sets:

> concat :: Ord a => Set (Set a) -> Set a
> concat = foldr union empty

Specialisation of 'sequence' from Control.Monad.

> sequence :: Ord a => [Set a] -> Set [a]
> sequence = List.foldr sequence' (singleton [])
>   where sequence' term = concat . map (flip map term . flip (:))

Analogous to 'liftM2'..

> lift2 :: Ord c => (a -> b -> c) -> Set a -> Set b -> Set c
> lift2 fn a b = concat $ flip map a (flip map b . fn)

Analagous to 'liftM4'..

> lift3 :: Ord d => (a -> b -> c -> d) -> Set a -> Set b -> Set c -> Set d
> lift3 fn a b c = concat . flip map a $ \a' ->
>   concat . flip map b $ \b' ->
>     flip map c $ fn a' b'
