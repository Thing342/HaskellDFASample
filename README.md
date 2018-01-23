# Implementation of a DFA in Haskell
A sample DFA has been provided.

To run:
```{sh}
gchi dfa.hs
```
Then, in GHCi:
```
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( dfa.hs, interpreted )
Ok, modules loaded: Main.
*Main> check mydfa "abababababa"
Left (Q3,"Bad final state")
```