# Installing

Sphinx is tested with continuous integratation for the ghc compiler versions
7.6, 7.8, and 7.10, along with cabal 1.18, 1.20, and 1.22. Sphinx has few
dependencies and they should all compile on major platforms (at the very least
Linux, Windows, and Mac OSX)

The [Haskell platform](https://www.haskell.org/platform/) contains all the
necessary tools to compile Sphinx. Most Linux distributions have recent
enough versions for ghc and cabal directly in their package manager. You can
install the Haskell platform from [chocolatey](https://chocolatey.org/) for
Windows and [brew](http://brew.sh/) for Mac OSX.

Once you have ghc and cabal installed, use the following commands at the
root of the code to compile Sphinx:

    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests
    cabal build

On recent versions of Ubuntu, install cabal and ghc with:

    $ sudo apt-get install ghc cabal-install libghc-cabal-dev

