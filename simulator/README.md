# ely: For simulating users of a multi-armed bandit

Half of Elysia. Named after *Elysia Chlorotica* the [majestic slug](https://en.wikipedia.org/wiki/Elysia_chlorotica).

To use: install [haskell Stack](https://docs.haskellstack.org/en/stable/README/). Then,

```
cd ely
stack build
stack ghci # Open an interactive REPL
:l Main # Load the main module
printSampleUsers # Generate some users/preferences
runTest # Hit the backend with a single user
```
