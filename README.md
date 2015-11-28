# Faun

[![Build status](https://travis-ci.org/PhDP/Sphinx-AI.svg?branch=master)](https://travis-ci.org/PhDP/Sphinx-AI)
[![Build status](https://ci.appveyor.com/api/projects/status/2g9tn9oprxm58gc6/branch/master?svg=true)](https://ci.appveyor.com/project/PhilippeDesjardinsProulx/manticore/branch/master)

Faun is a fun functional library for hybrid statistical + relational learning.
It focuses on the problem of structure learning: being able to learn theories
from data.

Faun has a companion project, Ofelia, a command-line tool to access Faun's 
most important algorithms.

See this [blog post](http://phdp.github.io/posts/2015-07-13-srl-code.html) for a full example.

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
