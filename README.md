# Manticore
[![Build status](https://travis-ci.org/PhDP/Manticore.svg?branch=master)](https://travis-ci.org/PhDP/Manticore)
[![Build status](https://ci.appveyor.com/api/projects/status/2g9tn9oprxm58gc6/branch/master?svg=true)](https://ci.appveyor.com/project/PhilippeDesjardinsProulx/manticore/branch/master)

Hybrid logic/probabilistic machine learning library.
A library for probabilistic logic (or statistical relational) learning
algorithms.

See this [blog post](http://phdp.github.io/posts/2015-07-13-srl-code.html) for a full example.

## Building

Using Cabal, ghc's package manager, you can build the library with:

    $ cabal update
    $ cabal install
    $ cabal build

Se *INSTALL.md* for platform-specific info.

After the library is compiled, simply type

    $ cabal repl

...to have access to the library in an interactive console.

The code is fully documented, type 

    $ cabal haddock

to generate a local copy of the documentation.

## Name

It's a hybrid and it answers queries: it **had** to be named *Sphinx*... but
there's already a package with this name in hackage, so another mythological
creature, the manticore, would have to do. It's a hybrid and it rhymes with
'multicore'.

## License

[MIT](http://opensource.org/licenses/MIT), see **LICENSE.md**.
