> {-# LANGUAGE LambdaCase #-}
>
> module Example.Clinic (clinic, Health(..), Symptom(..), generator) where
>
> import AI.Markov.HMM (HMM(..), observe)
> import System.Random (StdGen(..))

This example has been cribbed wholesale from [Wikipedia's page on the 
_Viterbi Algorithm_](https://en.wikipedia.org/wiki/Viterbi_algorithm).

Consider a model health clinic. Out-patients are either healthy or
feverous:

> data Health = Healthy | Fever
>
> instance Show Health where
>   show Healthy = "Healthy"
>   show Fever   = "Fever"

Doctors diagnose patients based on reported symptoms. On each visit,
patients report one of three health conditions; either that they feel
normal, cold, or dizzy:

> data Symptom = Normal | Cold | Dizzy
>
> instance Show Symptom where
>   show Normal = "Normal"
>   show Cold   = "Cold"
>   show Dizzy  = "Dizzy"

The health of some patient can be treated as a discrete hidden Markov
process. On each visit, the doctor can observe reports of symptoms,
using these to diagnose the patient's health (the hidden state.) 

The clinic know the population's general health condition, the prior
likelihood of changing between health conditions, and the symptoms cited
with or without fever on average. 

Represented in Haskell:

> clinic :: HMM Health Symptom
> clinic = HMM
>   { states  = [Healthy, Fever]
>   , symbols = [Normal, Cold, Dizzy]
>   , start   = [(Healthy, 0.6), (Fever, 0.4)]
>   , transition = \case
>       Healthy -> [(Healthy, 0.7), (Fever, 0.3)]
>       Fever   -> [(Healthy, 0.4), (Fever, 0.6)]
>   , emission = \case
>       Healthy -> [(Normal, 0.5), (Cold, 0.4), (Dizzy, 0.1)]
>       Fever   -> [(Normal, 0.1), (Cold, 0.3), (Dizzy, 0.6)]
>   }

Drawing values from a Distribution depends upon the availability of a 
random number generator; using the StdGen instance in System.Random, we 
produce a deterministic value with the required constraint:

> seed :: StdGen
> seed  = read $ unlines
>   [ "We're driving to the famous land some call"
>   , "Posterity, some famine, some the valley"
>   , "Of bones, valley of bones, valley of dry"
>   , "Bones where there is no heat nor hope nor dwelling:"
>   , "But cold security, the one and only"
>   , "Right of the workless man without a home."
>   ]

Our parametrised HMM can be used to generate an observation sequence, using
the three comprising distributions. This is an infinite list, contextually
representing symptoms likely to be reported over a sequence of visits:

> generator :: HMM Health Symptom -> [Symptom]
> generator = observe seed
