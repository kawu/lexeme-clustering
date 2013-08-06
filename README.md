lexeme-clustering
=================

The package provides a lexeme-clustering method based on the information theory.
It is a Haskell implementation of the algorithm described in [1].


Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build lexeme-clustering.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].

To install the latest development version from github run

    cabal install

from the top-level repository directory.


Usage
=====

The lexeme-clustering package provides a `lexeme-clustering` command-line tool which
can be used to cluster a list of words specified in the input file.

Input
-----

The input data should consists of a list of words, each word in a separate line.
Words don't have to be given in any specific order and duplicates are acceptable.
For example:

    psalm
    jako
    rzecz
    piękna
    jako
    rzecz
    przyjemna
    patrząc
    gdzie
    miłość
    panuje
    wzajemna
    ...


Output
------

The program will output the set of generated lexemes, each lexeme presented
in a separate line as a space-separated list of forms.


Clustering
----------

To cluster the `input.txt` file use the following command:

    lexeme-clustering input.txt

The program takes several optional parameters which control the clustering
process, for example:
* `--freqmin`: N-gram frequency threshold.
* `--nmax`: Maximum n-gram length taken on account.
* `--eps`: Take empty suffix on account.
* `--normmut`: Normalize mutual information to a `[0, 1]` range.
  The `kappa` parameter has to be adapted accordingly.
* `--kappa`: Kappa parameter, i.e. minimal mutual information needed
  to join two suffix sets.

Run `lexeme-clustering --help` to see the complete list of program arguments.


References
==========

[1] Maciej Janicki, "A Lexeme-Clustering Algorithm for Unsupervised Learning of Morphology".

[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"
