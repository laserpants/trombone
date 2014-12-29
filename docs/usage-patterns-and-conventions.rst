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


.. sourcecode:: javascript

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

.. raw:: html

    <style> td .line-block { margin: 0 !important; } .wy-table-responsive { overflow: visible; } </style>


+-----------+---------------------------------+---------------------------------------------+
| Code      | Title                           | Explanation                                 | 
+===========+=================================+=============================================+
| **200**   | Ok                              | A normal response.                          |
+-----------+---------------------------------+---------------------------------------------+
| **202**   | Accepted                        | | This response type indicates that the     |
|           |                                 | | result is a collection (array). That is,  |
|           |                                 | | each individual response item must be     |
|           |                                 | | considered separately and no claim is made|
|           |                                 | | as to the state of success w.r.t. these.  |
|           |                                 | | See `Array Actions <Array Actions_>`_.    |
+-----------+---------------------------------+---------------------------------------------+
| **400**   | Bad Request                     | | The request contains malformed JSON       |
|           |                                 | | or is otherwise invalid.                  |
+-----------+---------------------------------+---------------------------------------------+
| **401**   | Unauthorized                    | HMAC authentication failed.                 |
+-----------+---------------------------------+---------------------------------------------+
| **404**   | Not Found                       | | No route matches the request, or the      |
|           |                                 | | record doesn't exist for the route. E.g., |
|           |                                 | | a ``SELECT * FROM tbl WHERE id = {{id}}`` |
|           |                                 | | query returning an empty result.          |
+-----------+---------------------------------+---------------------------------------------+
| **500**   | Internal Server Error           | | An error occured during processing of the |
|           |                                 | | request. Refer to the attached            |
|           |                                 | | `error code <Error Codes_>`_ for details. |
+-----------+---------------------------------+---------------------------------------------+
| **503**   | Service Unavailable             | The server is shutting down or restarting.  |
+-----------+---------------------------------+---------------------------------------------+

.. | **409**   | Conflict                       |
   +-----------+--------------------------------+
   |                                            |

.. _error-codes: 

Error Codes
-----------

@todo

::

    {
        "status"       : false,
        "error"        : "NOT_FOUND",
        "responseCode" : 404,
        "message"      : "Resource not found."
    }


========================================== ==================
Code                                       Comment
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


Arrays
------

@todo

::

    POST   /tag/task  >>  
    
        SELECT * FROM tasks_tags WHERE task_id IN ( {{ids}} )

(Note the brackets.)

::

    {
      "ids": [1,2,3,4,5,6,7,39] 
    }


Notes about HTTP Methods
------------------------

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


