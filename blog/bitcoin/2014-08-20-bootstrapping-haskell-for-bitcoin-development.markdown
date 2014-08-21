---
title: Bootstrapping Haskell for Bitcoin development
---

It's time to get our hands dirty. Let's begin bootstrapping our Haskell development environment. Before we proceed it's probably fair to note that it's _very_ early days for Bitcoin development using Haskell and like users of most languages one should expect the Bitcoin libraries to be changing rapidly.

Never the less the [authors](https://github.com/haskoin/haskoin/graphs/contributors) of [Haskoin](http://hackage.haskell.org/package/haskoin) have done a remarkable job implementing a Bitcoin library in Haskell from scratch &mdash; this includes most (if not all) the low-level [elliptic curve cryptography](http://hackage.haskell.org/package/haskoin-0.1.0.2/docs/Network-Haskoin-Crypto.html) functionality. They are distributing the library with a public domain license but their aim is to build a [commercial wallet service](http://haskoin.com/) on top of the library. Please consider signing up for their service to show them your support.

Now I will show you how to create a sandboxed environment for your Bitcoin projects. This will reduce the risk of dependency conflicts (a.k.a. dependency hell) by installing a _sandboxed_ version of all the dependencies independent of other projects. Later we will see how we can share a sandbox between different Bitcoin projects. We will also likely require the bleeding edge development version of Haskoin pulled from [GitHub](https://github.com/haskoin/haskoin) and in due time I'll show you how to link the development version to your sandboxed project.

<!--more-->

In Haskell packages and dependencies are published to [Hackage](http://hackage.haskell.org/) and managed by [Cabal](http://www.haskell.org/cabal/) and since version 1.18 it has included support for sandboxed environments.

    $ mkdir test-haskoin    
    $ cd test-haskoin
    $ cabal sandbox init    [1]
    $ cabal update          [2]

<small>_[1] Initializes the sandbox and stores files in .cabal-sandbox_</small> \
<small>_[2] Download the package index from Hackage_</small>

Under normal circumstances we would be able to add Haskoin as a dependency in our _.cabal_ file but since Haskoin depends on the `C` `icu` library which is sometimes installed outside the default `C` library locations (like `/usr/local/opt`) we either need to include the library location in the _.cabal_ file or install the library manually with the extra command line parameters `--extra-include-dirs` and `--extra-lib-dirs`. 

On an *OS X* it is easiest to install the library using `brew` but please refer to the [*icu download page*](http://site.icu-project.org/download/) for installation instructions for your operating system of choice.

    $ brew install icu4c
    $ cabal install --extra-include-dirs=/usr/local/opt/icu4c/include \
                    --extra-lib-dirs=/usr/local/opt/icu4c/lib text-icu

Next we _cabalize_ our `test-haskell` project. 

    $ cabal init

This takes us to a project wizard. Answering the questions is relatively straight forward. The resulting project file &mdash; _test-haskell.cabal_ &mdash; describes the project and its dependencies.

Add Haskoin as a dependency to the _.cabal_ file.

    build-depends:       base >=4.6 && <4.7, haskoin ==0.1.0.*

Uncomment the following line by removing the two dashes.

    -- main-is:         Main.hs

Make sure that *Main.hs* exists with this content.

    module Main where

    main :: IO ()
    main = putStrLn "If this prints we're all done :)"

The last step is to install the dependencies, compile the project and hopefully see our achievement printed to _stdout_.

    $ cabal configure
    $ cabal install --dependencies-only
    $ cabal build
    $ cabal run

Actually, running `cabal run` will usually be enough as it will trigger the install and build events automatically.

In my next post in this series we will experiment with our sandboxed, cabalized Haskoin environment.
