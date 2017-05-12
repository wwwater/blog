# Haskell/Elm Blog App 
### A web application for a personal blog
Initially inspired by https://github.com/rundis/albums

Currently hosted on https://cyclinginthewind.com/


---

### To run locally:
#### Backend

`stack clean; stack build; stack exec cyclinginthewind`

`stack test --coverage --ghc-options "-fforce-recomp"`


#### Frontend

`npm run dev`

`elm test`
