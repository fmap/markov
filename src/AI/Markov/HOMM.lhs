\begin{code}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module AI.Markov.HOMM where

import Prelude hiding (map, sum)
import AI.Markov.HMM (HMM(..), HMMable(..))
import Data.Set (Set, map)
import Data.Set.Extras (lift2, lift3, (<~>),sum)
import Data.Distribution (Distribution, (?>))

data MM2 state = MM2
  { mm2States     :: Set state
  , mm2Start      :: Distribution (state, state)
  , mm2Transition :: (state, state) -> Distribution state
  }

instance Ord state => HMMable (MM2 state) (state, state) (state, state) where
  toHMM MM2{..} = HMM
    { hmmStates     = states
    , hmmSymbols    = states
    , hmmStart      = mm2Start
    , hmmTransition = transition
    , hmmEmission   = transition
    } where states = lift2 (,) mm2States mm2States
            transition = \(a, b) -> states <~> \(c, d) ->
              (,) (c, d) (if b /= c then 0 else mm2Transition (a, b) ?> d)
  fromHMM HMM{..} = MM2
    { mm2States     = states
    , mm2Start      = hmmStart
    , mm2Transition = transition
    } where states = map fst hmmStates
            transition = \(a, b) -> states <~> \s -> do
              let c = sum . map (\((_,o), p) -> if o == s then p else 0) $ hmmTransition (a,b)
              (s, c)

data MM3 state = MM3
  { mm3States     :: Set state
  , mm3Start      :: Distribution (state, state, state)
  , mm3Transition :: (state, state, state) -> Distribution state
  }

instance Ord state => HMMable (MM3 state) (state, state, state) (state, state, state) where
  toHMM MM3{..} = HMM
    { hmmStates     = states
    , hmmSymbols    = states
    , hmmStart      = mm3Start
    , hmmTransition = transition
    , hmmEmission   = transition
    } where states = lift3 (,,) mm3States mm3States mm3States
            transition = \(a, b, c) -> states <~> \(d, e, f) ->
              (,) (d, e, f) (if b /= d || c /= e then 0 else mm3Transition (a, b, c) ?> f)
  fromHMM HMM{..} = MM3
    { mm3States     = states
    , mm3Start      = hmmStart
    , mm3Transition = transition
    } where states = map (\(x,_,_) -> x) hmmStates
            transition = \(a, b, c) -> states <~> \s -> do
              let x = sum . map (\((_,_,o),p) -> if o == s then p else 0) $ hmmTransition (a, b, c)
              (s, x)
\end{code}
