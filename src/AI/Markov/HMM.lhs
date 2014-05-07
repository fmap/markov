> {-# LANGUAGE RecordWildCards, ViewPatterns, GeneralizedNewtypeDeriving, TemplateHaskell #-}
>
> module AI.Markov.HMM (HMM(..), observe, evaluate, sequenceP) where
>
> import Control.Applicative ((<$>))
> import Control.Monad (forM)
> import Data.Bifunctor (Bifunctor(first))
> import Data.Distribution (Distribution(..), Probability, (<?), (?>), (<~~))
> import Data.Function.Memoize (Memoizable(..), deriveMemoize, deriveMemoizable, memoize4)
> import Data.List.Extras (pairs, argmax)
> import Data.Ratio (Ratio)
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
     signal source, we can compare the accuracy of models.

We first consider a straightforward (albeit intractable) approach;
computing the likelihood our observations were produced by each possible
state sequence (of appropriate length), and summing the result
probabilities. I.e: $$P(O|HMM)=\sum_{j=0}^{n}P(O|I_n,HMM)P(I_n|HMM)$$

First, some precursors; given a set of states, `sequencesOfN` finds all
$n$-length state sequences:

> sequencesOfN :: Int -> HMM state symbol -> [[state]]
> sequencesOfN n = sequence . replicate n . states

`sequenceP` determines the likelihood of a state sequence, given a model;
$P(I|HMM)=P(i_1)P(i_2|i_1)\ldots P(i_r|i_{r-1})$:

> sequenceP :: Eq state => HMM state symbol -> [state] -> Probability
> sequenceP HMM{..} sequence = product 
>                            $ head sequence <? start 
>                            : map (uncurry (?>) . first transition) (pairs sequence)

`sequenceObservationsP` computes the likelihood of some state sequence 
and an observation sequence co-occuring; $P(O|I, HMM)=P(O_1|i_1)P(O_2|i_2) 
\ldots P(O_n|i_n)$:

> sequenceObservationsP :: Eq symbol => HMM state symbol -> [(state, symbol)] -> Probability
> sequenceObservationsP HMM{..} = product . map (uncurry (?>) . first emission)

Using the above primitives, we can now express the procedure:

> inefficientEvaluate :: (Eq state, Eq symbol) => HMM state symbol -> [symbol] -> Probability
> inefficientEvaluate hmm observations = sum $ zipWith (*) statesP statesObsP
>   where states     = sequencesOfN (length observations) hmm
>         statesP    = sequenceP hmm <$> states
>         statesObsP = sequenceObservationsP hmm <$> map (`zip` observations) states

But this has abysmal runtime-performance! Let $N$ be the number of
states, and $T$ $T$ equal to the number of observations (and thus the
length of each state sequence); there are thus $N^T$ possible state
sequences, and for each sequence we require about $2T$ calculations 
(Rabiner's [^rabiner1989] figure, I haven't validated this); meaning 
$2TN^T$ calculations in total: 
  
  $N$   $T$   $2TN^T$
  ----  ----  ------------------
  5     100   $\approx 10^{72}$
  10    100   $\approx 10^{102}$
  15    100   $\approx 10^{120}$

Given this profile, this function is only included here for didactic
purposes, and is not exported by this module.

A more efficient solution exists in the _forward algorithm_, which
arranges the computation so that redundant calculations may be cached: 

Given a partial observation sequence $\bold{O} = {O_1,O_2,\ldots,O_n}$ and
a terminal state $T$, the _forward variable_ provides the likelihood of
having observed $\bold{O}$ and being in state $T$ after time $n$ --
$\alpha_n(T) = P(O_1,O_2,\ldots,O_n,I_n=T|HMM)$:

> forwardVariable' :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> forwardVariable' 0 HMM{..} observations state = (state <? start) * (head observations <? emission state)
> forwardVariable' t hmm@HMM{..} observations state = (*) (observations !! t <? emission state) . sum $ do
>   predecessor <- states
>   let a = forwardVariable (t-1) hmm observations predecessor
>       b = state <? transition predecessor
>   return $ a * b

Below, we compute the terminal forward variable for each state in the HMM,
using a constant set of observations. As the computation of `a` is independent
of the state under consideration, it's time-saving to memoise this value:

> forwardVariable :: (Enum state, Bounded state, Eq state, Eq symbol, Memoizable state, Memoizable symbol) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> forwardVariable = memoize4 forwardVariable'
>
> instance (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Memoizable (HMM state symbol) where
>   memoize = $(deriveMemoize ''HMM)
>
> deriveMemoizable ''Ratio

As the final observation must be emitted in some (unknown) state, taking the
sum of terminal forward variables for each state yields our desired probability
-- $P(O_1,O_2,\ldots,O_n|HMM) = \sum_{n=1}^{|I|} \alpha_{|\bold{O}|}(I_n)$:

> forward :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => HMM state symbol -> [symbol] -> Probability
> forward hmm@HMM{..} observations = sum [forwardVariable t hmm observations state | state <- states]
>   where t = pred $ length observations
>
> evaluate :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => HMM state symbol -> [symbol] -> Probability
> evaluate = forward

This evaluation algorithm requires about $TN^2$ calculations, making it
many orders of magnitudes more efficient than the naïve approach:

  $N$   $T$   $TN^2$
  ----  ----  -------
  5     100   2,500
  10    100   10,000
  15    100   22,500

  2. Inspection: uncovering the hidden part of the model; from an
     observation sequence and a model, uncover the state sequence best
     explaining those observations, as provided by some optimality
     criterion.

There are several possible ways of solving this problem: there are
several possible optimality criteria, as the specification is ambiguous
as to the definition of an optimal state sequence.

One possible criterion involves maximising the expected number of
correct individual states: computing at each point in time, the most
likely state given the observation sequence and model.

To implement this solution, we need implement a variant of the forward
variable (namely, the backward variable), that describes the likelihood
of observing a sequence of succeeding observations from some known state
-- $\beta_n(T) = P(O_{n+1},O_{n+2},\ldots,O_{N}|I_n=T,HMM)$:

> backwardVariable' :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> backwardVariable' n HMM{..} observations state = if succ n == length observations then 1 else sum $ do
>   successor <- states
>   let a = transition state ?> successor
>       b = emission successor ?> (observations !! n)
>       c = backwardVariable (n+1) HMM{..} observations successor
>   return $ a * b * c
>
> backwardVariable :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> backwardVariable = memoize4 backwardVariable'

To recap: the forward variable determines the likelihood of
reaching some state $T$ being reached at time $n$, and a sequence
$O_1,O_2,\ldots,O_n$ being observed preceding it. The backward
variable accounts for the likelihood of the some state $T$ being
reached at time $n$, and the succeeding observation sequence being
$O_{n+1},O_{n+2},\ldots,O_{N}$.

Between them we can compute, provided a model and observation sequence,
the likelihood of some state $i$ co-occuring with any observation--the
smoothed probability value: $\gamma_n(i)=\frac{\alpha_n(T)\beta_n(T)}
{\sum^{N}_{s=1}\alpha_n{s}\beta_n{s}}$

> forwardBackwardVariable :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> forwardBackwardVariable n hmm observations state = forwardVariable n hmm observations state 
>                                                  * backwardVariable n hmm observations state
> 
> smooth :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> smooth n hmm@HMM{..} observations state = numerator / denominator
>   where numerator         = forwardBackward n state
>         denominator       = sum $ zipWith forwardBackward [1..] states
>         forwardBackward n = forwardBackwardVariable n hmm observations

By maximising the smoothing value at each position in the sequence, we
can find the most likely state at each position:

> mostLikelyState :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => HMM state symbol -> [symbol] -> Int -> state
> mostLikelyState hmm@HMM{..} observations position = argmax (smooth position hmm observations) states
>
> forwardBackward :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => HMM state symbol -> [symbol] -> [state]
> forwardBackward hmm observations = mostLikelyState hmm observations <$> [1..length observations]

This is a bad criterion, though; in considering only individual states,
we neglect information about the probability that state sequences will
occur. Consider the case in which a HMM has state transitions with zero
probability: the optimal state sequence may not even be valid! For this
reason, as before, this function is not exported by this module.

  3. Training: given some observation sequence, determine the parameters of
     some HMM that best model the data. If we so adapt model parameters to
     observed training data, we can accurately simulate real signal sources.

  [^rabiner1989]: Lawrence R.  Rabiner, _A Tutorial on Hidden Markov Models and
                  Selected Applications in Speech Recognition_, Proceedings of
                  the IEEE 77 (1989): 257-286
