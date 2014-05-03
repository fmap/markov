> module Main (main) where
>
> import AI.Markov.HMM (HMM(..), sequenceP, evaluate)
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

The evaluate function should predict the likelihood some sequence of
observations were produced by that HMM; $P(O_1,O_2,\ldots,O_n|HMM)$. 

> evaluateTests :: [(String, Bool)]
> evaluateTests = let test = evaluate clinic in first ("evaluate: " ++) <$>
>   [ ("Likelihood of [N] for clinic."   , test [Normal] == 0.6*0.5+0.4*0.1) 
>   , ("Likelihood of [N,N] for clinic." , test [Normal,Normal] == 0.6*0.5*0.7*0.5+0.6*0.5*0.3*0.1+0.4*0.1*0.4*0.5+0.4*0.1*0.6*0.1)
>   ]

To run our assertions:

> main :: IO ()
> main = runAssertions $ concat
>   [ sequencePTests
>   , evaluateTests
>   ]
