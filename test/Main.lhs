> module Main (main) where
>
> import AI.Markov.HMM (HMM(..), sequenceP)
> import Control.Applicative ((<$>))
> import Data.Bifunctor (Bifunctor(first))
> import Example.Clinic (clinic, Health(..), Symptom(..))
> import Test.Assert (runAssertions)

`sequenceP` should determine the likelihood of a state sequence, given a model;
$P(I|HMM)=P(i_1)P(i_2|i_1)\ldots P(i_r|i_{r-1})$:

> sequencePTests :: [(String, Bool)]
> sequencePTests = let test = sequenceP clinic in first ("sequenceP: " ++) <$>
>   [ ("Properly evaluates start probabilities."      , test [Healthy] == 0.6)
>   , ("Properly evaluates transition probabilities." , test [Healthy, Fever] == 0.6*0.3)
>   ] 

To run our assertions:

> main :: IO ()
> main = runAssertions $ concat
>   [ sequencePTests
>   ]
