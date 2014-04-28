> {-# LANGUAGE RecordWildCards #-}
>
> module AI.Markov.HMM (HMM(..), observe) where
>
> import Data.Distribution (Distribution(..), (<~~))
> import System.Random (RandomGen(..))
> import System.Random.Extras (split3)

_Hidden Markov models_ (HMMs) are used to model generative sequences
characterisable by a doubly-embedded stochastic process in which the
underlying process is hidden, and can only be observed through an
upper-level process, which produces output. 

Structurally:

You know what a Mealy machine is. A discrete hidden Markov model is
structurally similar to a Mealy machine, except that its transitions and
output values are governed by probability distributions, rather than
tokens from an input alphabet. Rather than having a fixed initial state,
this is chosen by an experiment on the prior distribution for the state
set; a set of conditional probability distributions determine the
likelihood of transition to any state from a given state, and another
set the conditional probability of emitting each symbol in the output
alphabet.

More formally, a HMM is a five-tuple consisting of:

> data HMM state symbol = HMM

  1. The distinct states of the process, S.

>   { states :: [state]

  2. A finite dictionary of possible observations, O.

>   , symbols :: [symbol]

  3. An initial state distribution, the likelihood of the process
     starting in each state s ∈ S.

>   , start :: Distribution state

  4. A transition distribution; given some current state i ∈ S, this
     provides the likelihood of the process next transiting to any
     j ∈ S. By the Markov assumption, this probability is dependent only
     on i.

>   , transition :: state -> Distribution state

  5. The observation symbol distributions; given some s ∈ S, this
     provides the likelihood of every observation o ∈ O being observed
     at that i. By the independence assumption, the output observation
     at any time is dependent only on the current state.

>   , emission :: state -> Distribution symbol
>   }

Having characterised some sequence generator as a HMM (and so given
a well-parametrised configuration of such a model), we can simulate
its output without collecting any further data; inferring future
behaviour from demonstrated statistical properties. This is particularly
useful in cases where gathering raw data is expensive. 

> observe :: RandomGen seed => seed -> HMM state symbol -> [symbol]
> observe seed hmm@HMM{..} = observe' ns hmm (start <~~ ts)
>   where (ts, ns) = split seed
>
> observe' :: RandomGen seed => seed -> HMM state symbol -> state -> [symbol]
> observe' seed hmm@HMM{..} state = obs : observe' s2 hmm nxt
>   where (s0,s1,s2) = split3 seed
>         (obs, nxt) = (emission state <~~ s0, transition state <~~ s1)

Rabiner [^rabiner1989] outlined three fundamental inference problems for HMMs:

  1. Evaluation: given a model and a sequence of observations, compute the
     likelihood those observations were produced by that HMM. This can also be
     interpreted as a scoring problem -- given a sequence produced by the real
     signal source, how well does the HMM model its behaviour?

  2. Inspection: uncovering the hidden part of the model; from an observation
     sequence and a model, uncover the state sequence best explaining those
     observations, as provided by some optimality criterion.

  3. Training: given some observation sequence, determine the parameters of
     some HMM that best model the data. If we so adapt model parameters to
     observed training data, we can accurately simulate real signal sources.

  [^rabiner1989]: Lawrence R.  Rabiner, _A Tutorial on Hidden Markov Models and
                  Selected Applications in Speech Recognition_, Proceedings of
                  the IEEE 77 (1989): 257-286
