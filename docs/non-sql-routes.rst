Non-SQL Routes
==============

This is an overview of the various route types that are not interacting directly with the database.

============ =================================================================================
Symbol       Explanation
============ =================================================================================
``||``       A request pipeline. (Followed by a pipeline identifier.)
``|>``       An inline request pipeline. (Followed by a pipeline definition.)
``<js>``     A node.js route. (Followed by a file path to the script.)
``{..}``     A static route. (Followed by a JSON object.)
============ =================================================================================


Pipelines
---------

| *The pipeline construction is a simple, declarative technique for composition of routes using JavaScript syntax.*

Pipelines can be declared in two different ways; either in a separate file or as inline definitions.

Pipeline configuration file
***************************

Inline pipeline syntax
**********************


Basic Format
************

Structure of a pipeline
```````````````````````

::

    GET  /my-pipeline  |>
    {
        "processors": [
        ],
        "connections": [
        ]
    }

Processors
``````````

@todo

Connections
```````````

@todo

Filters
*******

@todo

Equal-to
````````

@todo

Not-equal-to
````````````

@todo

Greater-than
````````````

@todo

Greater-than-or-equal
`````````````````````

@todo

Less-than
`````````

@todo

Less-than-or-equal
``````````````````

@todo

Transformers
************

@todo

Exclude
```````

@todo

Include
```````

@todo

Bind
````

@todo

Rename
``````

@todo

Copy
````

@todo

Aggregate
`````````

@todo

node.js
-------

http://nodejs.org/

Example 1.
**********

::

    GET /stuff  <js>  node/demo1.js


.. sourcecode:: javascript

    // node/demo1.js

    var response = {
        statusCode : 200,
        body       : 'Just saying "hello".' 
    };
    
    console.log(JSON.stringify(response));
 

Example 2.
**********


::

    POST /oracle <js>  node/demo2.js


.. sourcecode:: javascript

    // node/demo2.js

    var fs = require('fs');
    
    function parseStdin() {
        var data = fs.readFileSync('/dev/stdin').toString();
        if (data) {
            return JSON.parse(data);
        } else {
            return null;
        }
    };
    
    // Parse request object 
    var obj = parseStdin();
    
    // Do some heavy computation
    obj.string = obj.string.replace(/\%1/, '42');
    
    // Send response
    var response = {
        statusCode : 200,
        body       : obj
    };
    
    console.log(JSON.stringify(response));


::

    $ curl http://localhost:3010/oracle -d '{"string": "The answer is %1."}'
    The answer is 42.


Static Objects
--------------

The ``{..}`` syntax enables for static JSON response objects to be embedded directly in the route description.

::

    GET /stuff  {..}  {"status":"Ok.","response":[1,2,3,4]}


A possible use-case for this is to deliver machine readable documentation as part of a service (self-describing APIs), where participants automatically can determine their abilities against a communication endpoint using the ``OPTIONS`` HTTP method. See, e.g., http://zacstewart.com/2012/04/14/http-options-method.html for a discussion of this approach.

    | *At the very least, services should be responding with a 200 and the Allow header. That's just correct web server behavior. But there's really no excuse for JSON APIs not to be returning a documentation object.*
    
::

    OPTIONS /photo  {..}  {"GET":{"description":"Retreive a list of all photos."},
                           "POST":{"description":"Create a new photo."}}


The rationale for the ``OPTIONS`` method is outlined in `RFC 2616, Section 9.2 <http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html>`_.

    | *The OPTIONS method represents a request for information about the communication options available on the request/response chain identified by the Request-URI. This method allows the client to determine the options and/or requirements associated with a resource, or the capabilities of a server, without implying a resource action or initiating a resource retrieval.*

Special <Allow> keyword
***********************

Static JSON response routes support a special ``<Allow>`` keyword, the primary intent of which is to support the interaction pattern described above. 

::

    OPTIONS /photo  {..}  {"<Allow>":"GET,POST,OPTIONS",
                "GET":{"description":"Retreive a list of all photos."},
                "POST":{"description":"Create a new photo."}}


A typical response would then be:

::

    < HTTP/1.1 200
    < Allow: 'GET,POST,OPTIONS'
    < Content-Type: application/json; charset=utf-8
    {"GET":{"description":"Retreive a list of all customers."},
     "POST":{"description":"Create a new customer."}}

