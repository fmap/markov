> module Main (main) where
>
> import AI.Markov.HMM (sequenceP, evaluate, inspect)
> import Control.Applicative ((<$>))
> import Data.Bifunctor (Bifunctor(first))
> import Example.Doctor (doctor, Health(..), Symptom(..))
> import Test.Assert (runAssertions)

`sequenceP` should determine the likelihood of a state sequence, given a model;
$P(I|HMM)=P(i_1)P(i_2|i_1)\ldots P(i_r|i_{r-1})$:

> sequencePTests :: [(String, Bool)]
> sequencePTests = let test = sequenceP doctor in first ("sequenceP: " ++) <$>
>   [ ("Properly evaluates start probabilities."      , test [Healthy] == 0.6)
>   , ("Properly evaluates transition probabilities." , test [Healthy, Fever] == 0.6*0.3)
>   ] 

The evaluate function should predict the likelihood some sequence of
observations were produced by that HMM; $P(O_1,O_2,\ldots,O_n|HMM)$. 

> evaluateTests :: [(String, Bool)]
> evaluateTests = let test = evaluate doctor in first ("evaluate: " ++) <$>
>   [ ("Likelihood of [N] for doctor."   , test [Normal] == 0.6*0.5+0.4*0.1) 
>   , ("Likelihood of [N,N] for doctor." , test [Normal,Normal] == 0.6*0.5*0.7*0.5+0.6*0.5*0.3*0.1+0.4*0.1*0.4*0.5+0.4*0.1*0.6*0.1)
>   ]

The inspect function should find the state sequence most likely
responsible for some sequence of observations, given a HMM; 
$argmax_{q\inQ}P(q,O_1,O_2,\ldots,O_n|HMM)$: 

> inspectTests :: [(String, Bool)]
> inspectTests = let test = inspect doctor in first ("inspect: " ++) <$> 
>   [ ("Sequence behind [N] for doctor.", test [Normal] == [Healthy])
>   , ("Sequence behind [N,C,D] for doctor.", test [Normal, Cold, Dizzy] == [Healthy, Healthy, Fever])
>   ]

To run our assertions:

> main :: IO ()
> main = runAssertions $ concat
>   [ sequencePTests
>   , evaluateTests
>   , inspectTests
>   ]
