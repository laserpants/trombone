Installation
============

Build Prerequisites
-------------------

To build Trombone you need 

* the PostgreSQL database server, and pg_config (available from libpq-dev in Debian);

as well as

* the `Cabal tool <https://www.haskell.org/cabal/>`_  -- a build system for Haskell programs, and 
* a recent version of GHC (`The Glasgow Haskell Compiler <https://www.haskell.org/ghc/>`_).

Cabal and GHC come bundled with `the Haskell Platform <https://www.haskell.org/platform/>`_, which is available for all major operating systems. This is the recommended installation strategy, unless you have more specific requirements.

Getting the Haskell Platform
****************************

Consult the search utility provided by your distribution's package manager to locate a suitable candidate, or 
follow the instructions on the `download page <https://www.haskell.org/platform/`_ relevant to your operating system.

Building Trombone
*****************

Once you have GHC and Cabal installed, run the command 

.. sourcecode:: bash

    $ cabal update


to download the most recent list of packages. Next, clone the repository,

.. sourcecode:: bash

    $ git clone https://github.com/johanneshilden/trombone.git


and run the below sequence of commands. (The use of a sandbox here is optional, but recommended to avoid dependency problems.)

.. sourcecode:: bash

    $ cd trombone
    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal build




::

    dist/build/trombone/trombone


Troubleshooting
---------------

Report bugs and other issues to `github.com/johanneshilden/trombone/issues <http://github.com/johanneshilden/trombone/issues>`_.

