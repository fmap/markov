> {-# LANGUAGE FunctionalDependencies      #-}
> {-# LANGUAGE LambdaCase                  #-}
> {-# LANGUAGE MultiParamTypeClasses       #-}
> {-# LANGUAGE RecordWildCards             #-}
> {-# LANGUAGE TemplateHaskell             #-}
> {-# LANGUAGE TupleSections               #-}
> {-# LANGUAGE ViewPatterns                #-}
> {-# OPTIONS_GHC -fno-warn-orphans        #-}
> {-# OPTIONS_GHC -fno-warn-unused-binds   #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
> 
> module AI.Markov.HMM (
>   HMM(..),
>   observe,
>   sequenceP,
>   evaluate,
>   inspect,
>   train,
>   Limit(..),
>   uniformHMM,
>   HMMable(..)
> ) where
>
> import Control.Applicative ((<$>), (<*>), pure)
> import Data.Bifunctor (Bifunctor(first))
> import Data.Distribution (Distribution, Probability, (<?), (?>), (<~~), uniform)
> import Data.Function (on)
> import Data.Function.Extras (fixed)
> import Data.Function.Memoize (Memoizable(..), deriveMemoize, deriveMemoizable, memoize4)
> import Data.Functor.Infix ((<&>))
> import Data.Maybe (fromJust)
> import Data.List.Extras (pairs, argmax, argsum)
> import Data.Ratio (Ratio)
> import System.Random (RandomGen(..))
> import System.Random.Extras (split3)

_Hidden Markov models_ (HMMs) are used to model generative sequences
characterisable by a doubly-embedded stochastic process in which the
underlying process is hidden, and can only be observed through an
upper-level process, which produces output.

Structurally:

You know what a [Mealy machine][mealy] is, right? A discrete hidden Markov
model is structurally similar to a Mealy machine, except that its transitions
and output values are governed by probability distributions, rather than tokens
from an input alphabet. Rather than having a fixed initial state, this is
chosen by an experiment on the prior distribution for the state set; a set of
conditional probability distributions determine the likelihood of transition to
any state from a given state, and another set the conditional probability of
emitting each symbol in the output alphabet.

  [mealy]: https://en.wikipedia.org/wiki/Mealy_machine

More formally, a HMM is a five-tuple consisting of:

> data HMM state symbol = HMM

  1. The distinct states of the process, S. (|S| = N)

>   { hmmStates :: [state]

  2. A finite dictionary of possible observations, E.

>   , hmmSymbols :: [symbol]

  3. An initial state distribution, the likelihood of the process
     starting in each state s ∈ S.

>   , hmmStart :: Distribution state

  4. A transition distribution; given some current state i ∈ S, this
     provides the likelihood of the process next transiting to any
     j ∈ S. By the Markov assumption, this probability is dependent only
     on i.

>   , hmmTransition :: state -> Distribution state

  5. The observation symbol distributions; given some s ∈ S, this
     provides the likelihood of every observation o ∈ E being observed
     at that i. By the independence assumption, the output observation
     at any time is dependent only on the current state.

>   , hmmEmission :: state -> Distribution symbol
>   }

Having characterised some sequence generator as a HMM (and so given a
well-parametrised configuration of such a model), we can simulate its
output without collecting any further data; inferring future behaviour
from demonstrated statistical properties. This is particularly useful in
cases where gathering raw data is expensive.

> observe :: RandomGen seed => seed -> HMM state symbol -> [symbol]
> observe seed hmm@HMM{..} = observe' ns hmm (hmmStart <~~ ts)
>   where (ts, ns) = split seed
>
> observe' :: RandomGen seed => seed -> HMM state symbol -> state -> [symbol]
> observe' seed hmm@HMM{..} state = obs : observe' s2 hmm nxt
>   where (s0,s1,s2) = split3 seed
>         (obs, nxt) = (hmmEmission state <~~ s0, hmmTransition state <~~ s1)

Rabiner [^rabiner1989] outlined three fundamental inference problems for HMMs:

  1. Evaluation: given a model and a sequence $(O_n)_{n=1}^T$ of observations, compute the
     likelihood those observations were produced by that HMM. This can also be
     interpreted as a scoring problem -- given a sequence produced by the real
     signal source, we can compare the accuracy of models.

We first consider a straightforward (albeit intractable) approach;
computing the likelihood our observations were produced by each possible
state sequence $(I_n)_{n=1}^T$ (of appropriate length), and summing the result
probabilities. I.e: $$P(O|HMM)=\sum_{j=0}^{n}P(O|I_n,HMM)P(I_n|HMM)$$

First, some precursors; given a set of states, `sequencesOfN` finds all
$n$-length state sequences:

> sequencesOfN :: Int -> HMM state symbol -> [[state]]
> sequencesOfN n = sequence . replicate n . hmmStates

`sequenceP` determines the likelihood of a state sequence, given a model;
$P(I|HMM)=P(I_1)P(I_2|I_1)\ldots P(I_r|I_{r-1})$:

> sequenceP :: Eq state => HMM state symbol -> [state] -> Probability
> sequenceP HMM{..} sequence = product 
>                            $ head sequence <? hmmStart
>                            : map (uncurry (?>) . first hmmTransition) (pairs sequence)

`sequenceObservationsP` computes the likelihood of an observation sequence 
given a state sequence; $P(O|I, HMM)=P(O_1|I_1)P(O_2|I_2) 
\ldots P(O_n|I_n)$:

> sequenceObservationsP :: Eq symbol => HMM state symbol -> [(state, symbol)] -> Probability
> sequenceObservationsP HMM{..} = product . map (uncurry (?>) . first hmmEmission)

Using the above primitives, we can now express the procedure:

> inefficientEvaluate :: (Eq state, Eq symbol) => HMM state symbol -> [symbol] -> Probability
> inefficientEvaluate hmm observations = sum $ zipWith (*) statesP statesObsP
>   where states     = sequencesOfN (length observations) hmm
>         statesP    = sequenceP hmm <$> states
>         statesObsP = sequenceObservationsP hmm <$> map (`zip` observations) states

But this has abysmal runtime-performance! Let $N$ be the number of
states, and $T$ equal to the number of observations (and thus the
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
a terminal state $i$, the _forward variable_ provides the likelihood of
having observed $\bold{O}$ and being in state $i$ after time $n$ --
$\alpha_n(i) = P(O_1,O_2,\ldots,O_n,I_n=i|HMM)$:

> forwardVariable' :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> forwardVariable' 0 HMM{..} observations state = (state <? hmmStart) * (head observations <? hmmEmission state)
> forwardVariable' t hmm@HMM{..} observations state = (*) (observations !! t <? hmmEmission state) . sum $ do
>   predecessor <- hmmStates
>   let a = forwardVariable (t-1) hmm observations predecessor
>       b = state <? hmmTransition predecessor
>   return $ a * b

Below, we compute the terminal forward variable for each state in the HMM,
using a constant set of observations. As the computation of `a` is independent
of the state under consideration, it's time-saving to memoise this value:

> forwardVariable :: (Enum state, Bounded state, Eq state, Eq symbol, Memoizable state, Memoizable symbol) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> forwardVariable = memoize4 forwardVariable'
>
> $(return []) -- Since GHC 7.8, nothing is within scope of the first splice.
>
> instance (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Memoizable (HMM state symbol) where
>   memoize = $(deriveMemoize ''HMM)
>
> deriveMemoizable ''Ratio

As the final observation must be emitted in some (unknown) state, taking the
sum of terminal forward variables for each state yields our desired probability
-- $P(O_1,O_2,\ldots,O_n|HMM) = \sum_{n=1}^{|I|} \alpha_{|\bold{O}|}(I_n)$:

> forward :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => HMM state symbol -> [symbol] -> Probability
> forward hmm@HMM{..} observations = forwardVariable t hmm observations `argsum` hmmStates
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
-- $\beta_n(i) = P(O_{n+1},O_{n+2},\ldots,O_{T}|I_n=i,HMM)$:

> backwardVariable' :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> backwardVariable' n HMM{..} observations state = if succ n == length observations then 1 else sum $ do
>   successor <- hmmStates
>   let a = hmmTransition state ?> successor
>       b = hmmEmission successor ?> (observations !! n)
>       c = backwardVariable (n+1) HMM{..} observations successor
>   return $ a * b * c
>
> backwardVariable :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> backwardVariable = memoize4 backwardVariable'

To recap: the forward variable determines the likelihood of
reaching some state $i$ being reached at time $n$, and a sequence
$O_1,O_2,\ldots,O_n$ being observed preceding it. The backward
variable accounts for the likelihood of the same state $i$ being
reached at time $n$, and the succeeding observation sequence being
$O_{n+1},O_{n+2},\ldots,O_{T}$.

Between them we can compute, provided a model and observation sequence,
the likelihood of some state $i$ given the observation sequence--the
smoothed probability value: 

  \begin{align*}
    \gamma_n(i)
      \\ &=\frac{\alpha_n(i)\beta_n(i)}{\sum^{N}_{s=1}\alpha_n{s}\beta_n{s}} 
      \\ &=\frac{P(I_n=i,O_1,O_2,\ldots,O_T|HMM)}{\sum^{N}_{s=1} P(I_n=s,O_1,O_2,\ldots,O_T|HMM)} 
      \\ &=\frac{P(I_n=i,O_1,O_2,\ldots,O_T|HMM)} {P(O_1,O_2,\ldots,O_T|HMM)} 
      \\ &=P(I_n=i|O_1,O_2,\ldots,O_T,HMM)
  \end{align*}

> forwardBackwardVariable :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> forwardBackwardVariable n hmm observations state = forwardVariable n hmm observations state 
>                                                  * backwardVariable n hmm observations state
> 
> smooth :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> Probability
> smooth n hmm@HMM{..} observations state = numerator / denominator
>   where numerator         = forwardBackward n state
>         denominator       = sum $ zipWith forwardBackward [0..] hmmStates
>         forwardBackward n = forwardBackwardVariable n hmm observations

By maximising the smoothing value at each position in the sequence, we
can find the most likely state at each position:

> forwardBackward :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => HMM state symbol -> [symbol] -> [state]
> forwardBackward hmm@HMM{..} observations = do
>   position <- [0..pred $ length observations]
>   return $ argmax (smooth position hmm observations) hmmStates

This is a bad criterion, though; in considering only individual states,
we neglect information about the probability that state sequences will
occur. Consider the case in which a HMM has state transitions with zero
probability: the optimal state sequence may not even be valid! For this
reason, as before, this function is not exported by this module.

A more reasonable optimality criterion is to find the most probable
contiguous sequence of states; i.e. determining the state sequence that
maximises $P(O,I|HMM)$. This can be found by application of _the Viterbi
algorithm_, which involves maximising over likelihood estimates for each
possible state sequence.

Given an observation sequence $\bold{O} = {O_1,O_2,\ldots,O_T}$ a state
$i$, the _Viterbi step_ finds the state sequence most likely to account
for the first $n$ observations and terminating at state $i$; that is:
$$\delta_n(i) = \argmax_{I_1,I_2,\ldots,I_{n-1}} P(I_1,I_2,\ldots,I_n=1,
\bold{O}|HMM)$$:

> viterbiStep' :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> ([state], Probability)
> viterbiStep' 0 HMM{..}     observations state = (pure state, hmmStart ?> state * hmmEmission state ?> head observations)
> viterbiStep' n hmm@HMM{..} observations state = argmax snd $ do
>   predecessor <- hmmStates
>   let (path, prob) = viterbiStep (n-1) hmm observations predecessor
>       likelihood   = prob * hmmTransition predecessor ?> state * hmmEmission state ?> (observations !! n)
>   return $ (state:path, likelihood)
>
> viterbiStep :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> ([state], Probability)
> viterbiStep = memoize4 viterbiStep'

As the final observation must be emitted in some state, maximising the
Viterbi step over $S$ yields the desired state sequence:

> viterbi :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => HMM state symbol -> [symbol] -> [state]
> viterbi hmm@HMM{..} observations = reverse . fst . argmax snd $ do
>   let t = length observations - 1
>   terminal <- hmmStates
>   return $ viterbiStep t hmm observations terminal
>
> inspect :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => HMM state symbol -> [symbol] -> [state]
> inspect = viterbi

  3. Training: given some observation sequence, determine the parameters of
     some HMM that best model the data. If we so adapt model parameters to
     observed training data, we can accurately simulate real signal sources.

Solution: the _Baum-Welch algorithm_. Two precursors:

We define two HMMs to be equivalent if they share dictionaries and
probability distributions. Though we can't in general define equality
over `(->)`, here in both cases the domain is the state dictionary, and
so bounded, meaning this is computable.

> instance (Eq state, Eq symbol) => Eq (HMM state symbol) where
>   HMM st0 sy0 i0 t0 e0 == HMM st1 sy1 i1 t1 e1 = and
>     [ st0 == st1
>     , sy0 == sy1
>     , i0  == i1
>     , on (==) (`fmap` st0) t0 t1
>     , on (==) (`fmap` st0) e0 e1
>     ]

`xi` determines the likelihood of being in states $i$ and $j$, at times
$n$ and $n+1$ respectively, provided a HMM and observation sequence:

  \begin{align*}
    \xi_n(i,j)
      \\ &=P(I_n=i,I_{n+1}=j|O_1,O_2,\ldots,O_T,HMM) 
      \\ &=\frac{P(O_1,O_2,\ldots,O_T|I_n=i,I_{n+1}=j,HMM)}{P(O_1,O_2,\ldots,O_T|HMM)}
      \\ &=\frac{P(O_1,\ldots,O_{n-1}|I_n=i,I_{n+1}=j,HMM)P(O_n,\ldots,O_T|O_1,\ldots,O_{n-1},I_n=i,I_{n+1}=j,HMM)}{P(O_1,O_2,\ldots,O_T|HMM)}
      \\ &=\frac{P(O_1,\ldots,O_{n-1}|I_n=i,HMM)P(O_n,\ldots,O_T|I_n=i,I_{n+1}=j,HMM)}{P(O_1,O_2,\ldots,O_T|HMM)}
      \\ &=\frac{\alpha_n(i)P(O_n|I_n=i,I_{n+1}=j,HMM)P(O_{n+1},\ldots,O_T|O_n,I_n=i,I_{n+1}=j,HMM)}{P(O_1,O_2,\ldots,O_T|HMM)}
      \\ &=\frac{\alpha_n(i)P(O_n|I_n=i,HMM)P(I_{n+1}=j|I_n=i,HMM)P(O_{n+1},\ldots,O_T|I_{n+1}=j,HMM)}{P(O_1,O_2,\ldots,O_T|HMM)}
      \\ &=\frac{\alpha_n(i)P(O_n|I_n=i,HMM)P(I_{n+1}=j|I_n=i,HMM)\beta_{n+1}(j)}{\sum_{k \in S} \alpha_n(k) \beta_n(k)}
  \end{align*}

> xi :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> HMM state symbol -> [symbol] -> state -> state -> Probability
> xi n hmm@HMM{..} observations i j = forwardVariable n hmm observations i
>                                   * hmmTransition i ?> j
>                                   * backwardVariable (n+1) hmm observations j
>                                   * observations !! (n+1) <? hmmEmission j
>                                   / forwardBackwardVariable n hmm observations `argsum` hmmStates

Between `xi` and the above smoothing function, we can re-estimate HMM
parameters:

> step :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => HMM state symbol -> [symbol] -> HMM state symbol
> step hmm@HMM{..} observations = hmm
>   { hmmStart = hmmStates <&> (,) <*> smooth 1 hmm observations
>   , hmmTransition = \from -> hmmStates <&> \to -> (to,)
>       $ argsum (\n -> xi n hmm observations from to) [0..t-1]
>       / argsum (\n -> smooth n hmm observations from) [0..t-1]
>   , hmmEmission = \from -> hmmSymbols <&> \emission -> (emission,)
>       $ argsum (\n -> b2i (observations!!n == emission) * smooth n hmm observations from) [0..t] 
>       / argsum (\n -> smooth n hmm observations from) [0..t]
>   } where (t,b2i) = (length observations - 1, \case { True -> 1; False -> 0})

The _Baum-Welch algorithm_ involves iteratively applying the above
re-estimation until attainment of a fixed point. Though convergence is
guaranteed for this procedure, we allow optional provision of a limit
for time sensitive applications -- if re-restimation is still yielding
improvements after this number of iterations, we cut off execution:

> newtype Limit = Limit { limit :: Int }
>
> baumWelch' :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Int -> Int -> HMM state symbol -> [symbol] -> HMM state symbol
> baumWelch' limit iterations hmm observations 
>   | iterations >= limit = hmm
>   | hmm' == hmm         = hmm
>   | otherwise           = baumWelch' limit (succ iterations) hmm observations
>   where hmm' = hmm `step` observations
>
> baumWelch :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Maybe Limit -> HMM state symbol -> [symbol] -> HMM state symbol
> baumWelch Nothing = flip (fixed . flip step)
> baumWelch (limit . fromJust -> limit) = baumWelch' limit 0 
>
> train :: (Memoizable state, Memoizable symbol, Eq state, Eq symbol, Enum state, Bounded state) => Maybe Limit -> HMM state symbol -> [symbol] -> HMM state symbol
> train = baumWelch

Note: the Baum-Welch procedure is not guaranteed to achieve a global
maximum, and so it is possible to over-fit a particular data set.

The above HMM training procedure is re-estimative: provided a model and
an observation sequence, we find a model that better or equivalently
describes the observations. But what if we're missing initial
parameters? In cases in which prior information is unavailable, a
sensible option is to set the parameters to be uniform, that is:

> uniformHMM :: [state] -> [symbol] -> HMM state symbol
> uniformHMM states symbols = HMM
>   { hmmStates     = states
>   , hmmSymbols    = symbols
>   , hmmStart      = uniform states
>   , hmmTransition = const $ uniform states
>   , hmmEmission   = const $ uniform symbols
>   } 

First-order HMMs are flexible enough to be used to represent simple Markov
processes (without hidden states), as well as higher-order HMMs (in which we
relax the Markov assumption -- transition probabilities can depend on more than
one prior state. In this variant, the start distribution is the prior
distribution for each m-length sequence of states:

  * Higher-order HMMs can be represented using first-order models by mapping
    the alphabet from individual state values to m-tuples of those states,
    where each tuple represents a sequence of states.

      data HMM2 state symbol = HMM2
        { hmm2States     :: [state]
        , hmm2Symbols    :: [symbol]
        , hmm2Start      :: Distribution (state, state)
        , hmm2Transition :: (state, state) -> Distribution (state, state)
        , hmm2Emission   :: (state, state) -> Distribution symbol
        }

      toHMM :: HMM2 state symbol -> HMM state symbol
      toHMM HMM2{..} = HMM
        { hmmStates     = liftM2 (,) hmm2States hmm2States
        , hmmSymbols    = hmm2Symbols
        , hmmStart      = hmm2Start
        , hmmTransition = hmm2Transition
        , hmmEmission   = hmm2Emission
        }

      HMM2.observe :: RandomGen seed => seed -> HMM2 state symbol -> [symbol]
      HMM2.observe seed = observe seed . toHMM

  * To represent a simple Markov process with a HMM, construct the HMM with
    common state and symbol dictionaries, and the emission distribution such
    that states produce their corresponding symbol with certainty.

      data MM state = MM
        { mmStates     :: [state]
        , mmStart      :: Distribution state
        , mmTransition :: state -> Distribution state
        }

      toHMM :: MM state -> HMM state state
      toHMM MM{..} = HMM
        { hmmStates      = mmStates
        , hmmSymbols     = mmStates
        , hmmTransition  = mmTransition
        , hmmEmission    = \position -> mmStates <&> \state -> (state, if state =  = position then 1 else 0)
        }

      MM.observe :: RandomGen seed => seed -> MM state -> [state]
      MM.observe seed = observe seed . toHMM

    Which themselves can be mapped to higher-order MMs, analogously to the
    procedure for higher-order HMMs.

At the cost of efficiency, we can write considerably less code! To this end, we
here define a typeclass to capture translations between HMMs and analagous
structures:

> class HMMable process state symbol | process -> state symbol where
>   toHMM   :: process -> HMM state symbol
>   fromHMM :: HMM state symbol -> process

  [^rabiner1989]: Lawrence R.  Rabiner, _A Tutorial on Hidden Markov Models and
                  Selected Applications in Speech Recognition_, Proceedings of
                  the IEEE 77 (1989): 257-286
