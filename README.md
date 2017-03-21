# Haskell/Elm Blog App 
### A web application for a personal blog
Based on https://github.com/rundis/albums

stack clean
stack build
stack exec cyclinginthewind
stack test --coverage --ghc-options "-fforce-recomp"
