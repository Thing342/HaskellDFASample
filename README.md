# Implementation of a DFA and an NFA in pure Haskell
Sample code for both is available at `dfa_sample.hs` and `nfa_sample.hs`, respectively.

To play with the samples:
```{sh}
gchi dfa_sample.hs
```
Then, in GHCi:
```
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( dfa.hs, interpreted )
Ok, modules loaded: Main.
*Main> check mydfa "abababababa"
Left (Q3,"Bad final state")
```
