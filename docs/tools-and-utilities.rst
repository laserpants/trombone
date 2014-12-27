Tools & Utilities
=================

Console
-------

.. image:: console.png 

Keyman
------

The ``keyman`` utility implements a simple CRUD interface, suitable for command line administration of client keys. 

:: 

    Usage:
      keyman list [--config=<file>]
      keyman (register|renew) <client> [<key>] [--config=<file>]
      keyman revoke <client> [--config=<file>]
      keyman --help
    
    Options:
      -c --config=<file>  Path to database connection file.
      -? --help           Display this help.


The configuration file contains a list of parameters (identical to those `described here <http://www.postgresql.org/docs/9.1/static/libpq-connect.html>`_.) used to establish a database connection. Note that the default location for this file is ``~/.config/trombone/keyman.conf``.

Sample ``keyman.conf`` file:

::

    host     = 'localhost' 
    port     =  5432 
    dbname   = 'trombone' 
    user     = 'postgres' 
    password = 'postgres'


Keyman usage
````````````

To list existing client keys:

.. sourcecode:: bash

        $ ./keyman list

        generic            : 14ad0ef86bf392b38bad6009113c2a5a8a1d993a
        batman             : 53d5864520d65aa0364a52ddbb116ca78e0df8dc
        spock              : 78a302b6d3e0e37d2e37cf932955781900c46eca
 
        
Register a new client:

.. sourcecode:: bash

        $ ./keyman register my_application

        Client registered:
        my_application: 53d5864520d65aa0364a52ddbb116ca78e0df8dc
    

A token is automatically generated for the new client. Alternatively, an existing key (a 40 character long hexadecimal string) may be specified as an extra, trailing argument: ``keyman register my_application 53d5864520d65aa0364a52ddbb116ca78e0df8dc``. Subsequent to registering the application, we can confirm that it appears in the client list with its new key.
    

.. sourcecode:: bash

    $ ./keyman list | grep my_application

    my_application      : 53d5864520d65aa0364a52ddbb116ca78e0df8dc
 

To remove a client, use:
    

.. sourcecode:: bash

    $ ./keyman revoke unwanted_client



JavaScript libraries
--------------------

::

    trombone.request.js
    trombone.request.min.js


@todo
