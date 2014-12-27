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

+-----------+------------------------------+
| Code      | Explanation                  |
+===========+==============================+
| 200       | Ok                           |
+-----------+------------------------------+
| A normal response.                       |
+-----------+------------------------------+
| 202       | Accepted                     |
+-----------+------------------------------+
|                                          |
+-----------+------------------------------+
| 400       | Bad Request                  |
+-----------+------------------------------+
| The request contains malformed JSON      |
| or is otherwise invalid.                 |
+-----------+------------------------------+
| 401       | Unauthorized                 |
+-----------+------------------------------+
| HMAC authentication failed.              |
+-----------+------------------------------+
| 404       | Not Found                    |
+-----------+------------------------------+
| - No route matches the request.          |
| - The selected record does not exist     |
|   for the route. E.g., a                 |
|   ``SELECT * FROM tbl WHERE id = {{id}}``|
|   query returns an empty result.         |
+-----------+------------------------------+
| 409       | Conflict                     |
+-----------+------------------------------+
|                                          |
+-----------+------------------------------+
| 500       | Internal Server Error        |
+-----------+------------------------------+
|                                          |
+-----------+------------------------------+
| 503       | Service Unavailable          |
+-----------+------------------------------+
| The server is shutting down or           |
| restarting.                              |
+-----------+------------------------------+


Error Codes
-----------

::

    {
        "status"       : false,
        "error"        : "NOT_FOUND",
        "responseCode" : 404,
        "message"      : "Resource not found."
    }


========================================== ==================
Code                                       Explanation
========================================== ==================
``BAD_REQUEST``
``NOT_FOUND``
``UNAUTHORIZED``
``CONFLICT``
``SQL_FOREIGN_KEY_CONSTRAINT_VIOLATION``
``SQL_UNIQUE_CONSTRAINT_VIOLATION``
``SQL_ERROR``
``SERVER_CONFIGURATION_ERROR``
``SERVICE_UNAVAILABLE``
``INTERNAL_SERVER_ERROR``
========================================== ==================


HTTP Methods
------------

@todo

GET
***

POST
****

PUT
***

DELETE
******

Idempotency in a nutshell
`````````````````````````

OPTIONS
*******


PATCH
*****


