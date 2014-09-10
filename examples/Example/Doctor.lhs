> {-# LANGUAGE LambdaCase                 #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE TemplateHaskell            #-}
> {-# LANGUAGE OverloadedLists            #-}
>
> module Example.Doctor (doctor, Health(..), Symptom(..), generator, evaluator, inspector, trainer) where
>
> import AI.Markov.HMM (HMM(..), observe, evaluate, inspect, train)
> import Data.Distribution (Probability)
> import Data.Function.Memoize (deriveMemoizable)
> import Data.Set (fromList)
> import System.Random (StdGen, mkStdGen)

This example has been cribbed wholesale from [Wikipedia's page on the
_Viterbi Algorithm_](https://en.wikipedia.org/wiki/Viterbi_algorithm).

Consider a doctor, who's patients are either healthy or feverous:

> data Health = Healthy | Fever
>   deriving (Eq, Ord, Enum, Bounded)
>
> instance Show Health where
>   show Healthy = "Healthy"
>   show Fever   = "Fever"

Patients are diagnosed based on their reported symptoms. On each visit,
patients report one of three health conditions; either that they feel
normal, cold, or dizzy:

> data Symptom = Normal | Cold | Dizzy
>   deriving (Eq, Ord, Enum, Bounded)
>
> instance Show Symptom where
>   show Normal = "Normal"
>   show Cold   = "Cold"
>   show Dizzy  = "Dizzy"

The health of some patient can be treated as a discrete hidden Markov
process. On each visit, the patient reports his symptoms to the doctor,
who uses these to diagnose their health (which cannot be observed
directly.)

The doctor knows the population's general health condition, the prior
likelihood of each change in health conditions between each visit, and
the symptoms cited with or without fever on average.

Represented in Haskell:

> doctor :: HMM Health Symptom
> doctor = HMM
>   { hmmStates  = fromList [Healthy, Fever]
>   , hmmSymbols = fromList [Normal, Cold, Dizzy]
>   , hmmStart   = fromList [(Healthy, 0.6), (Fever, 0.4)]
>   , hmmTransition = fromList . \case
>       Healthy -> [(Healthy, 0.7), (Fever, 0.3)]
>       Fever   -> [(Healthy, 0.4), (Fever, 0.6)]
>   , hmmEmission = fromList . \case
>       Healthy -> [(Normal, 0.5), (Cold, 0.4), (Dizzy, 0.1)]
>       Fever   -> [(Normal, 0.1), (Cold, 0.3), (Dizzy, 0.6)]
>   }

Drawing values from a Distribution depends upon the availability of a
random number generator; using the StdGen instance in System.Random, we
produce a deterministic value with the required constraint:

> seed :: StdGen
> seed  = mkStdGen 0x29a

Our parametrised HMM can be used to generate an observation sequence, using
the three comprising distributions. This is an infinite list, contextually
representing symptoms likely to be reported over a sequence of visits:

> generator :: HMM Health Symptom -> [Symptom]
> generator = observe seed

Given a HMM and an observation sequence, we can use the forward procedure to
determine the likelihood of that sequence given a model. In cases where these
sequences are from a real signal source, this provides a measure of the model
validity; in context: how well the doctor understands patient behaviour:

> evaluator :: HMM Health Symptom -> [Symptom] -> Probability
> evaluator = evaluate
>
> deriveMemoizable ''Health
> deriveMemoizable ''Symptom

Given some HMM and an observation sequence, we can use the Viterbi
algorithm to determine the hidden states most likely underlying those
observations. These are the health conditions that best explain the
patient's symptoms at each point.

> inspector :: HMM Health Symptom -> [Symptom] -> [Health]
> inspector = inspect

In the the Baum-Welch procedure, given a set of observations and some existing
model, the model parameters are re-estimated so as to better explain the
observations. Contextuallt: a doctor revises his priors to more accurately
diagnose patients.

> trainer :: [Symptom] -> HMM Health Symptom
> trainer = train Nothing doctor
