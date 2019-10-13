## Installation Notes

By default, Blarney uses `ghc` via the user's system installation.  The Haskell community has two other popular ways to invoke `ghc`. This note helps the respective users get started with Blarney, quickly. We made sure that existing users can continue to run with their system `ghc` installation.

### Using [`ghc`](https://downloads.haskell.org/~ghc/8.6.1/docs/html/users_guide/intro.html)
Install `ghc`, follwing the 8.6.1 (or later) [instructions](https://downloads.haskell.org/~ghc/8.6.1/docs/html/users_guide/intro.html). Blarney's `Script/blc` command will then invoke `ghc --make`.


### Using [`stack`](https://docs.haskellstack.org/en/stable/README/)

There are two aspects here. Some users may want to use `blc` as before, but use the `ghc` installation implied by the `stack.yaml` file. Other users may want to completely switch to using `blarney.cabal` and `stack` for their builds.

#### Have `blc` use the `ghc` specified in `stack.yaml`

To make sure that `Scripts/blc` uses `ghc` as per the `stack.yaml` configuration, set `BLARNEY_BLC_USING_STACK` like so:

```
$ export BLARNEY_BLC_USING_STACK=1
```

`Scripts/blc` will now invoke ghc using `stack ghc -- --make`, instead of the direct `ghc --make` command.

As before, run the blarney tests by invoking
```
$ ./Test/test.sh
```

#### Replace `Script/blc` with `stack build`
The `stack build` command will build the Blarney libraries and all example/test binaries. The `Script/blc` command is not needed anymore and setting `BLARNEY_TEST_USING_STACK_BUILD` like so:

```
$ export BLARNEY_TEST_USING_STACK_BUILD=1
```
will make sure that `Test/test.sh` will run with the previously stack-built binaries. 


### Using [`cabal new-install`](https://www.haskell.org/cabal/download.html) (Cabal 3)

TODO
