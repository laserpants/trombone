Authentication
==============

Security model
--------------

To establish the authenticity of a request, the server must perform a message integrity check which operates on a cryptographic primitive known as a HMAC (hash-based message authentication code). A MAC is attached to each request, in the form of an ``API-Access`` header. During dispatch, a subsequent code is computed from the request object using a token (secure key) associated with the client application. The result of this operation is compared with the original MAC attached to the request, in order to verify its authenticity.

The key is a random, 40-character long, hexadecimal string.

::

    53d5864520d65aa0364a52ddbb116ca78e0df8dc

Table schema
************

A ``trombone_keys`` table is used to maintain client-key associations.

::

    CREATE TABLE trombone_keys (
        id serial,
        client character varying(40),
        key character varying(40)
    );

    ALTER TABLE ONLY trombone_keys
        ADD CONSTRAINT trombone_keys PRIMARY KEY (id);

    ALTER TABLE ONLY trombone_keys
        ADD CONSTRAINT unique_trombone_keys_client UNIQUE (client);


.. NOTE::
   This table is automatically created when the server is started with authentication enabled (i.e., in default mode), unless it already exists.

Registering client applications
*******************************

In order for a client application to be granted access to the service, it must;

1. be present in the ``trombone_keys`` table with a unique identifier and its secure token; as well as
2. supply the following HTTP header with each request:

:: 

    API-Access: <client>:<hash>


    
where ``<client>`` is replaced with the name used to identify the application, and ``<hash>`` with the MAC code obtained by hashing the request body using the `HMAC-SHA1 <http://en.wikipedia.org/wiki/SHA-1>`_ algorithm and aforementioned key .

SHA1 implementations are available for most programming languages. The following have been tested with Trombone:

========== ===============================================================       
JavaScript https://code.google.com/p/crypto-js/ 
Haskell    http://hackage.haskell.org/package/Crypto/docs/Data-HMAC.html
========== ===============================================================       

For complete, working examples, see `Reference Implementations`_.

Client key administration
`````````````````````````

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


The configuratoin file contains a list of parameters (identical to those `described here <http://www.postgresql.org/docs/9.1/static/libpq-connect.html>`_.) used to connect to the database. Default location for this file is ``~/.config/trombone/keyman.conf``.

Sample ``keyman.conf`` file:

::

    host     = 'localhost' 
    port     =  5432 
    dbname   = 'trombone' 
    user     = 'postgres' 
    password = 'postgres'


To list existing client keys:

:: 

        $ ./keyman list

        generic            : 14ad0ef86bf392b38bad6009113c2a5a8a1d993a
        batman             : 53d5864520d65aa0364a52ddbb116ca78e0df8dc
        spock              : 78a302b6d3e0e37d2e37cf932955781900c46eca
 
        
Register a new client:

::

        $ ./keyman register my_application

        Client registered:
        my_application: 53d5864520d65aa0364a52ddbb116ca78e0df8dc
    

A token is automatically generated for the new client. Alternatively, an existing key (a 40 character long hexadecimal string) may be specified as a trailing argument: ``keyman register my_application 53d5864520d65aa0364a52ddbb116ca78e0df8dc``. After registering an application, we can confirm that it appears in the client list with its new key.
    

::

    $ ./keyman list | grep my_application

    my_application      : 53d5864520d65aa0364a52ddbb116ca78e0df8dc
 

To remove a client, use:
    

::

    $ ./keyman revoke unwanted_client


.. comments
    A simple bash script, such as the one presented here (`utils/bash/keyadmin.sh <github.com/johanneshilden/trombone/blob/refactor/utils/bash/keyadmin.sh>`_), can be used to manage client keys.
    
    ::
    
        #!/bin/bash
        
        # Replace <database> below with name of database
        db="<database>"  
        
        # PostgreSQL user
        psql_user="postgres"
        
        # Modify according to host environment
        psql_cmd="sudo -u $psql_user psql -d $db -c" 
        
        case $1 in
            list)
                eval "$psql_cmd \"SELECT client, key FROM trombone_keys;\"" | tail -n+3 | head -n-2 | awk '{printf "%-20s %-40s\n", $1, $3}'
                ;;
            register)
                eval "$psql_cmd \"INSERT INTO trombone_keys (client, key) VALUES ('$2', encode(digest(random()::text, 'sha1'), 'hex'));\""
                ;;
            renew)
                eval "$psql_cmd \"UPDATE trombone_keys SET key = encode(digest(random()::text, 'sha1'), 'hex') WHERE client = '$2';\""
                ;;
            revoke)
                eval "$psql_cmd \"DELETE FROM trombone_keys WHERE client = '$2';\""
                ;;
            *)
                echo "Usage: $0 {list|register|renew|revoke} [client]"
                exit 1
        esac
    
    Edit the file as required; save, e.g. as ``keyadmin.sh``; and assign necessary permissions.
    
    ::
    
        chmod +x keyadmin.sh
    
    Then use the command as:
    
    ::
    
        $ ./keyadmin.sh list
    
        generic              14ad0ef86bf392b38bad6009113c2a5a8a1d993a
        batman               53d5864520d65aa0364a52ddbb116ca78e0df8dc
        spock                78a302b6d3e0e37d2e37cf932955781900c46eca
    
    ::
    
        $ ./keyadmin.sh register my_application
    
    A token is generated for the new client using ``encode(digest(random()::text, 'sha1'), 'hex')``. After registering an application, it appears in the client list with its new key.
    
    ::
    
        $ ./keyadmin.sh list | grep my_application
    
        my_application       53d5864520d65aa0364a52ddbb116ca78e0df8dc
    
    Similarly, to remove a client use:
    
    ::
    
        $ ./keyadmin.sh revoke unwanted_client

Disable HMAC authentication
***************************

Message authentication can be disabled with the ``-x`` command line switch. Doing so in a production environment is not recommended, since it renders the system vulnerable to unauthorized access.

.. WARNING::
   Deactivating message authentication gives everyone access to your server interface. To mitigate the risk of unauthorized access to production data, only use the ``-x`` flag in a safe environment.


Allowing access from localhost
``````````````````````````````

To bypass HMAC authentication specifically for requests originating from a local host, instead use the ``-t``, or ``--trust-localhost`` option. 

Reference Implementations
-------------------------

@todo

