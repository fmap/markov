20140505:0214
=============

First-order HMMs are flexible enough to be used to represent Markov
processes without hidden states, as well as higher-order HMMs:

  * To represent a simple Markov process with a HMM, construct the HMM
    with common state and symbol dictionaries, and the emission
    distribution such that states produce their corresponding symbol
    with certainty.

  * A higher-order Hidden Markov Model is one in which we relax the
    Markov assumption - in the hidden layer, the probability of next
    transitioning to any state is dependent on the previous m states, 
    where m is the order of the model. The start distribution in this
    variant models the likelihood of each m-length sequence.

    These can be represented using first-order models by mapping the
    alphabet from individual state values to to m-tuples of those
    values, where each tuple represents a sequence of states. For
    example, a 2nd order HMM for DNA can be represented as a first-order
    HMM with alphabet:

      ```haskell
      λ: let ns = "ACGT" in [(n,m) | n <- ns, m <- ns]
      [('A','A'),('A','C'),('A','G'),('A','T'),('C','A'),('C','C'),('C','G'),('C','T'),('G','A'),('G','C'),('G','G'),('G','T'),('T','A'),('T','C'),('T','G'),('T','T')]
      ```

It may be productive at some point to define data-types for simple Markov
processes and m-order HMMs (using type-level natural numbers?); exporting 
them with interfaces to our HMM algorithms (first translating using these
correspondence relations.)
