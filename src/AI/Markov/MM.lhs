> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE RecordWildCards       #-}
>
> module AI.Markov.MM (
>   MM(..),
>   observe,
>   sequenceP,
>   evaluate,
>   train,
>   uniformMM
> ) where
>
> import AI.Markov.HMM (HMM(..), HMMable(..), Limit, uniformHMM)
> import qualified AI.Markov.HMM as HMM (observe, evaluate, sequenceP, train)
> import Data.Function.Memoize (Memoizable(..))
> import Data.Functor.Infix ((<&>), (<$$>))
> import Data.Distribution (Distribution, Probability)
> import System.Random (RandomGen)

Structurally, a Markov model can be thought of as a finite state machine in
which state transitions are governed probability distributions. Rather than
having a fixed initial state, this is selected by experiment upon the prior
distribution 'mmStart'; instead of transitions being determined by tokens from
the an input alphabet, a set of probability distributions 'mmTransition'
determine the likelihood of transiting from any one state to another.

> data MM state = MM
>   { mmStates     :: [state]
>   , mmStart      :: Distribution state
>   , mmTransition :: state -> Distribution state
>   }

As you've already noticed, this is a restricted definition of the hidden Markov
models we've seen before. In fact: all hidden Markov processes are Markov
processes, and all Markov processes can be mapped to hidden Markov processes.

A Markov process is a hidden Markov process in which the state and symbol
dictionaries are are equivalent, and the emission distribution is such that
states produce their corresponding symbol with certainty. When we're only
concerned with state transitions, we can forgo the emission probabilities of
a HMM, and treat it as a MM.

We can exploit these morphisms by first instancing the HMMable typeclass, and
then defining functions for inspection, parameter estimation, etc, for MMs in
terms of their HMM implementations.

Look:

> instance Eq state => HMMable (MM state) state state where
>   toHMM MM{..} = HMM
>     { hmmStates      = mmStates
>     , hmmSymbols     = mmStates
>     , hmmStart       = mmStart
>     , hmmTransition  = mmTransition
>     , hmmEmission    = \position -> mmStates <&> \state -> (state, if state == position then 1 else 0)
>     }
>   fromHMM HMM{..} = MM
>     { mmStates     = hmmStates
>     , mmStart      = hmmStart
>     , mmTransition = hmmTransition
>     }
>
> observe :: Eq state => RandomGen seed => seed -> MM state -> [state]
> observe seed = HMM.observe seed . toHMM
>
> sequenceP :: Eq state => MM state -> [state] -> Probability
> sequenceP = HMM.sequenceP . toHMM
>
> evaluate :: Memoizable state => Eq state => Enum state => Bounded state => MM state -> [state] -> Probability
> evaluate = HMM.evaluate . toHMM
>
> train :: Memoizable state => Eq state => Enum state => Bounded state => Maybe Limit -> MM state -> [state] -> MM state
> train limit = fromHMM <$$> HMM.train limit . toHMM
>
> uniformMM :: Eq state => [state] -> MM state
> uniformMM states = fromHMM $ uniformHMM states states

Neat, right? At cost of efficiency, we saved a few days of research and programming.
