# Manticore
[![Build status](https://travis-ci.org/PhDP/Manticore.svg?branch=master)](https://travis-ci.org/PhDP/Manticore)
[![Build status](https://ci.appveyor.com/api/projects/status/2g9tn9oprxm58gc6/branch/master?svg=true)](https://ci.appveyor.com/project/PhilippeDesjardinsProulx/manticore/branch/master)

Hybrid logic/probabilistic machine learning library.
A library for probabilistic logic (or statistical relational) learning
algorithms.

## Building

Using Cabal, ghc's package manager, you can build the library with:

    $ cabal update
    $ cabal install
    $ cabal build

Then, simply type

    $ cabal repl

...to have access to the library in an interactive console.

The code is fully documented, type 

    $ cabal haddock

to generate a local copy of the documentation.

## Example

Here's a complete example based on the classic *smoking* example from the
paper that introduced Markov logic (M Richardson and P Domingos,
*Markov Logic Networks*, Machine Learning, 2006.).

First, start the console ('cabal repl' in the root of the directory) and
import the Markov logic network module:

    >>> import qualified Manticore.MarkovLogic as ML

The most straighforward way to build a Markov logic network is with the
*fromStrings* function from the Manticore.MarkovLogic module. This function
takes an array of strings, each of which must be a valid first-order logic
formula followed (or preceded) by a number (the weight of the formula). In this
case we have:

    >>> let mln = ML.fromStrings ["∀x Smoking(x) ⇒ Cancer(x) 1.5", "∀x∀y Friend(x, y) ∧ Smoking(x) ⇒ Smoking(y) 1.1"]

The strings were copy-pasted from Richardson and Domingos' paper, but the
parsers is flexible and will accept a keyboard-friendly form too:

    >>> let mln' = ML.fromStrings ["1.5 forall x Smoking(x) implies Cancer(x)", "1.1 forall x,y Friend(x, y) and Smoking(x) implies Smoking(y)"]

A Markov logic network is a template for Markov networks. To get a Markov
network, we need to apply a set of constant to the Markov logic networks.
Here, we will create only two constants:

    >>> let cs = ["Anna", "Bob"]

Then, we can query the network, say, what is the probability that Anna has
cancer?

    >>> ML.ask mln cs "P(Cancer(Anna))"
    Just 0.6133819604540808

The function 'Manticore.MarkovLogic.ask' takes a Markov logic network, a list
of terms (represented as a list of strings), and a string query. It will return
Just P, with P being a probability in the [0.0, 1.0] range, or Nothing it the
parser fails to read the query. To make the process a bit easier we'll create
a function ask that have already the mln and terms supplied:

    >>> let ask = MLN.ask mln cs

Then we can ask queries with this function:

    >>> ask "P(Cancer(Anna) and Cancer(Bob))"
    Just 0.38061259085226223

    >>> ask "P(Cancer(Anna) | Smoking(Bob))"
    Just 0.6519697695221907

    >>> ask "P(Cancer(Anna) | Smoking(Bob), Friend(Bob, Anna))"
    Just 0.7056438194691147

We could write "Cancer(Anna) = True", but the parser assumes all predicates
are true if they are not assigned to a value. If we can to say something is
false, say: Anna is not smoking, we could write "Smoking(Anna) = False" or
the shorter "!Smoking(Anna)".

    >>> ask "P(Cancer(Anna) | !Smoking(Anna), Smoking(Bob), Friend(Bob, Anna))"
    Just 0.49999999999999994

    >>> ask "P(Cancer(Anna) | !Smoking(Anna), !Smoking(Bob), !Friend(Bob, Anna))"
    Just 0.5000000000000002

## Name

It's a hybrid and it answers queries: it **had** to be named *Sphinx*... but
there's already a package with this name in hackage, so another mythological
creature, the manticore, would have to do. It's a hybrid and it rhymes with
'multicore'.

## License

[MIT](http://opensource.org/licenses/MIT), see **LICENSE.md**.
