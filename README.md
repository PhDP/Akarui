<p align='center'>
  <img src='https://phdp.github.io/images/logo-akarui.svg.png' alt='logo'/>
</p>

Akarui is a fun functional library for experimenting with different reasoning systems (read the doc
carefully: some stuff is experimental, some is fast, some is slow).

Development is done on the 'develop' branch.

## Building
[![Build Status](https://travis-ci.org/PhDP/Akarui.svg?branch=master)](https://travis-ci.org/PhDP/Akarui)

Using Cabal, ghc's package manager, you can build the library with:

    $ cabal new-update
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
