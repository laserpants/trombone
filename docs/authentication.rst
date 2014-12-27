Authentication
==============

Security model
--------------

To establish the authenticity of a request, the server must perform a message integrity check, operating on a cryptographic primitive known as a HMAC (hash-based message authentication code). A MAC is attached to each request, in the form of an ``API-Access`` header. During dispatch, a subsequent code is computed from the request object using 

- a token (secure key) associated with the client application, 
- an incremental nonce (see below), and 
- the request method together with the path info. 
  
The result of this operation is compared with the original MAC attached to the request, in order to verify its authenticity.

The key is a random, 40-character long, hexadecimal string.

::

    53d5864520d65aa0364a52ddbb116ca78e0df8dc


Table schema
************

The ``trombone_keys`` table maintains client-key associations.

.. sourcecode:: postgres

    CREATE TABLE trombone_keys (
        id serial,
        client character varying(40) NOT NULL,
        key character varying(40) NOT NULL,
        nonce bigint NOT NULL
    );

    ALTER TABLE ONLY trombone_keys
        ADD CONSTRAINT trombone_keys PRIMARY KEY (id);

    ALTER TABLE ONLY trombone_keys
        ADD CONSTRAINT unique_trombone_keys_client UNIQUE (client);


.. NOTE::
   This table is automatically created when the server starts with authentication enabled (i.e., in default mode), unless it already exists.

Authenticating client applications
**********************************

In order for a client application to be granted access to the service, it must;

1. be present in the ``trombone_keys`` table with a unique identifier and its secure token; as well as
2. supply the following HTTP header with each request:

:: 

    API-Access: <client_id>:<nonce>:<hash>


    
where ``<client_id>`` is replaced with the name of the application (as it appears in the ``trombone_keys`` table), and ``<hash>`` with the MAC code obtained by hashing a concatenated string -- the constituents of which are given below, using the `HMAC-SHA1 <http://en.wikipedia.org/wiki/SHA-1>`_ algorithm and aforementioned key.

The ``<nonce>`` is an integer value introduced to prevent an adversary from reusing a hash in a, so called, `replay attack <http://en.wikipedia.org/wiki/Replay_attack>`_. The client implementation must therefore ensure that the nonce is strictly increasing for each request. This can be achieved using a timestamp, such as the one used in the reference implementation.   

Hash string format
``````````````````

The format of the string given as input to the hashing algorithm must be as follows:

::

    <client_id>:<method>:<uri>:<nonce>:<json_body>


SHA1 implementations are available for most programming languages. The following have been tested with Trombone:

========== ===============================================================       
JavaScript https://code.google.com/p/crypto-js/ 
Haskell    http://hackage.haskell.org/package/Crypto/docs/Data-HMAC.html
========== ===============================================================       

For complete, working examples, see `Reference Implementations`_.

Client key administration
`````````````````````````

Trombone includes the ``keyman`` utility, which can be used for command line administration of client keys. 

See `Tools & Utilities <tools-and-utilities.html>`_.


Disable HMAC authentication
***************************

Message authentication can be disabled with the ``-x`` command line switch. Doing so in a production setting is not recommended.

.. WARNING::
   Deactivating message authentication gives everyone access to your server interface. To mitigate the risk of unauthorized access to production data, only use the ``-x`` flag in a safe environment.


Allowing access from localhost
``````````````````````````````

To bypass HMAC authentication specifically for requests originating from the local host, instead use the ``-t``, or ``--trust-localhost`` option. 

Reference Implementations
-------------------------

.. sourcecode:: psql

    CREATE DATABASE basic_auth_demo;
    
    \c basic_auth_demo
    

.. sourcecode:: postgres

    CREATE TABLE IF NOT EXISTS utilities (
        id        serial PRIMARY KEY,
        name      character varying(255)       NOT NULL,
        summary   character varying(255)       NOT NULL
    );
    
    INSERT INTO utilities (name, summary) VALUES 
        ('ls',   'list directory contents'),
        ('htop', 'interactive process viewer'),
        ('df',   'report file system disk usage'),
        ('pwd',  'print name of current/working directory'),
        ('awk',  'pattern scanning and text processing language');
    
    CREATE TABLE IF NOT EXISTS trombone_config (
        id        serial PRIMARY KEY, 
        key       character varying(40) UNIQUE NOT NULL, 
        val       text                         NOT NULL
    ); 
     
    INSERT INTO trombone_config (key, val) VALUES 
        ('routes', E'GET /utils >> SELECT * FROM utilities\nPOST /util <> INSERT INTO utilities (name, summary) VALUES ({{name}}, {{summary}})');


Create a file ``basic-keyman.conf``:

::

    host     = 'localhost' 
    port     =  5432 
    dbname   = 'basic_auth_demo' 
    user     = 'postgres' 
    password = 'postgres'


(Modify the file as required.)

.. sourcecode:: bash

    $ ./keyman register demo -c basic-keyman.conf

    Client registered:
    demo: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


Start the server

.. sourcecode:: bash

    $ trombone -d basic_auth_demo -C


JavaScript
**********

Insert the generated ``demo`` key on line 15.

.. sourcecode:: javascript
    :linenos:
    :emphasize-lines: 15

    // auth-example.js

    $(document).ready(function() {

        var render = function(obj) {
            $('#response').html('<pre>' + JSON.stringify(obj, null, 4) + '</pre>');
        };

        var onError = function(e) {
            render(JSON.parse(e.responseText));
        };

        var defaults = {
            host     : 'http://localhost:3010',
            key      : 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
            client   : 'demo',
            type     : 'GET',
            error    : onError
        };

        $('#insert-action').click(function() {

            var name    = $('#insert-title').val(),
                summary = $('#insert-description').val();

            if (!summary || !name) {
                $('#response').html('Please fill out both fields.');
                return;
            }

            var obj = {
                summary : summary,
                name    : name
            };

            Trombone.request($.extend({}, defaults, {
                data     : obj,
                nonce    : Date.now()/10 | 0,
                type     : 'POST',
                resource : 'util',
                success  : function() { 
                    $('#response').html('Ok.'); 
                }
            }));

        });

        $('#request-action').click(function() {

            Trombone.request($.extend({}, defaults, {
                nonce    : Date.now()/10 | 0,
                resource : 'utils',
                success  : render
            }));

        });
    });


.. sourcecode:: html

    <!DOCTYPE html>
    <html lang="en">
        <head>
            <meta charset="utf-8">
            <title>Trombone data access service example: Request authentication</title>
        </head>
        <body>
    
            <div>
                <a id="request-action" href="javascript:">Request some data</a>
            </div>
            <div>
                <div><input id="insert-title" type="text"></div>
                <div><textarea id="insert-description"></textarea></div>
                <div><a id="insert-action" href="javascript:">Insert some data</a></div>
            </div>
            <div id="response"></div>

            <script src="http://code.jquery.com/jquery-2.1.1.min.js"></script>
            <script src="http://crypto-js.googlecode.com/svn/tags/3.1.2/build/rollups/aes.js"></script>
            <script src="http://crypto-js.googlecode.com/svn/tags/3.1.2/build/rollups/hmac-sha1.js"></script>
            <script src="js/trombone.request.min.js"></script>
            <script src="js/auth-example.js"></script>
        </body>
    </html>


Haskell
*******

@todo

Purescript
**********

@todo

C++/Qt
******

@todo

