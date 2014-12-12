Installation
============

Build Prerequisites
-------------------

To build Trombone you need the `Cabal tool <https://www.haskell.org/cabal/>`_  -- a build system for Haskell programs, and a recent version of GHC (`The Glasgow Haskell Compiler <https://www.haskell.org/ghc/>`_).

Both of these come bundled with the Haskell Platform, which is available for all major operating systems. This is also the recommended method of installation, unless you have more specific requirements.

Getting the Haskell Platform
****************************

Consult the search tool provided by your distribution's package manager to locate a suitable candidate, or follow the instructions on the official web page for instructions relating to your system:

https://www.haskell.org/platform/

Building
********

Once you have GHC and Cabal installed, run the command 

::

    $ cabal update


to download the most recent list of packages. Next, clone the repository,

::

    $ git clone https://github.com/johanneshilden/trombone.git


and run the below sequence of commands. (The use of a sandbox here is optional, but recommended to avoid dependency problems.)

::

    $ cd trombone
    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal build




::

    dist/build/trombone/trombone


Troubleshooting
---------------

It happens that builds fail,

