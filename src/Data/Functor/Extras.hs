module Data.Functor.Extras ((<$$>), for) where

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f(g(a)) -> f(g(b))
(<$$>) = fmap fmap fmap
infixl 4 <$$>

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap
