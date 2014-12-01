Basic Configuration
===================

Running
-------

Ping
****

::

    curl localhost:3010/ping

Unix signal handlers
--------------------

.. sourcecode:: bash

    kill -SIGHUP `ps -a | awk '/trombone/ {print $1}'`

Cloud-friendly config. storage
------------------------------

