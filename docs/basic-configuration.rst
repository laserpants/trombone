Basic Configuration
===================

Running
-------

To start the service 

* on port 3010 (default),
* with the configuration file ``my.conf``, 
* connecting to the database ``my_database``, 
  
run the following command:

::

    ./trombone -d my_database -r my.conf


For a complete list of flags and switches, see `Command Line Flags <command-line-flags.html>`_. 

Ping
****

To send a ping request to the server, we may then use a command line tool like ``curl``:

::

    $ curl localhost:3010/ping


A typical response (if the service is running):

::

    < HTTP/1.1 200 
    < Transfer-Encoding: chunked
    < Content-Type: application/json; charset=utf-8
    < Server: Trombone/0.8
    {
       "status":true,
       "message":"Pong!"
    }



Unix signal handlers
--------------------

.. sourcecode:: bash

    kill -SIGHUP `ps -a | awk '/trombone/ {print $1}'`

Configuration data storage
--------------------------

.. In cloud-based architectures, file system storage is typically short lived (ephemeral) and resources assigned to an application are reclaimed by the platform when the service is stopped or restarted. 

The server will look for a database table called ``trombone_config`` as a fallback, when a configuration file is not specified (i.e., the ``-r`` flag is omitted). This is useful if you prefer to store configuration data in the database. 

::

    CREATE TABLE IF NOT EXISTS trombone_config (
        id   serial                PRIMARY KEY, 
        key  character varying(40) UNIQUE        NOT NULL, 
        val  text                                NOT NULL
    );
 
.. NOTE::
   This table is automatically created when the server starts, unless it already exists.


