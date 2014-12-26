Usage Patterns & Conventions
============================

Naming
------

Trombone makes two fairly idiomatic assumptions; namely that,

* database tables and columns follow the ``lowercase_separated_by_underscores`` naming convention, and that 
* JSON objects use ``camelCase`` formatting. 
  
Conversion between these two formats is usually implicit.

Array Actions
-------------

::

    curl http://localhost:3010 --verbose -d '[{}, {}]'


::

    curl http://localhost:3010 --verbose -d '[{"summary":"","name":""}, {"summary":"","name":""}, {"summary":"","name":""}]'


::

    var obj = [
        {
            name: 'Object #1',
            summary: '...'
        },
        {
            name: 'Object #2',
            summary: '...'
        },
        {
            name: 'Object #3',
            summary: '...'
        }
    ];

    Trombone.request({
        host     : 'http://localhost:3010',
        client   : 'demo',
        key      : 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
        type     : 'POST',
        resource : 'util',
        data     : obj,
        nonce    : Date.now()/10 | 0,
        success  : function() { alert('Ok.'); }
    });


Response Codes
--------------

@todo

HTTP Methods
------------

@todo

GET
***

@todo

POST
****

@todo

PUT
***

@todo

DELETE
******

@todo

Idempotency in a nutshell
`````````````````````````

OPTIONS
*******

@todo

PATCH
*****

@todo

