# Faun

[![Build status](https://travis-ci.org/PhDP/Faun.svg?branch=master)](https://travis-ci.org/PhDP/Faun)

Faun is a fun functional library for experimenting with different reasoning
systems (read the doc carefully: some stuff is experimental, some is fast, some
is slow).

<<<<<<< HEAD
Development will move to 'develop' before the 1.0 release.
=======
Development is done on the 'develop' branch.
>>>>>>> 378b2391949b54f61a12280dde87f476e6aacf03

## Building

Using Cabal, ghc's package manager, you can build the library with:

    $ cabal new-install --lib
    $ cabal new-configure --enable-tests
    $ cabal new-build
    $ cabal new-test

After the library is compiled, simply type

    $ cabal new-repl

...to have access to the library in an interactive console.

The code is fully documented, type

    $ cabal new-haddock

to generate a local copy of the documentation.

## License

The code is released under the permissive [Apache v2](http://www.apache.org/licenses/LICENSE-2.0)
license, [along with an exception to ensure GPLv2 compatibility](https://lwn.net/Articles/701155/)
see **LICENSE.md**.
