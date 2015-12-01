# Faun

[![Build status](https://travis-ci.org/PhDP/Faun.svg?branch=master)](https://travis-ci.org/PhDP/Faun)
[![Build status](https://ci.appveyor.com/api/projects/status/2g9tn9oprxm58gc6/branch/master?svg=true)](https://ci.appveyor.com/project/PhilippeDesjardinsProulx/manticore/branch/master)

Faun is a fun functional library for experimenting with different Reasoning
systems.

See this [blog post](http://phdp.github.io/posts/2015-07-13-srl-code.html) for
an example of probabilistic logic.

## Building

Using Cabal, ghc's package manager, you can build the library with:

    $ cabal update
    $ cabal install
    $ cabal build

See *INSTALL.md* for platform-specific info.

After the library is compiled, simply type

    $ cabal repl

...to have access to the library in an interactive console.

The code is fully documented, type

    $ cabal haddock

to generate a local copy of the documentation.

## License

[MIT](http://opensource.org/licenses/MIT), see **LICENSE.md**.
