Non-SQL Routes
==============

This is an overview of the various route types that do not interact directly with the database.

============ =================================================================================
Symbol       Explanation
------------ ---------------------------------------------------------------------------------
``||``       A request pipeline. (Followed by a pipeline identifier.)
``|>``       An inline request pipeline. (Followed by a pipeline definition.)
``<js>``     A node.js route. (Followed by a file path to the script.)
``{..}``     A static route. (Followed by a JSON object.)
============ =================================================================================


Pipelines
---------

@todo

node.js
-------

@todo

Static Objects
--------------

The ``{..}`` syntax enables for static JSON response objects to be embedded directly in the route description.

::

    GET /stuff  {..}  {"status":"Ok.","response":[1,2,3,4]}


A possible use-case for this is to deliver machine readable documentation as part of the service (self-describing APIs), where clients automatically can determine their abilities against a communication endpoint using the ``OPTIONS`` HTTP method. See, e.g., http://zacstewart.com/2012/04/14/http-options-method.html for a discussion of this approach.

    | *At the very least, services should be responding with a 200 and the Allow header. That's just correct web server behavior. But there's really no excuse for JSON APIs not to be returning a documentation object.*
    
::

    OPTIONS /photo  {..}  {"GET":{"description":"Retreive a list of all photos."},"POST":{"description":"Create a new photo."}}


The rationale for the ``OPTIONS`` method is outlined in `RFC 2616, Section 9.2 <http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html>`_.

    | *The OPTIONS method represents a request for information about the communication options available on the request/response chain identified by the Request-URI. This method allows the client to determine the options and/or requirements associated with a resource, or the capabilities of a server, without implying a resource action or initiating a resource retrieval.*

Special <Allow> keyword
***********************

Static JSON response routes support a special ``<Allow>`` keyword, the primary intent of which is to support the interaction pattern described above. 

::

    OPTIONS /photo  {..}  {"<Allow>":"GET,POST,OPTIONS","GET":{"description":"Retreive a list of all photos."},"POST":{"description":"Create a new photo."}}


A typical response would then be:

::

    < HTTP/1.1 200
    < Allow: 'GET,POST,OPTIONS'
    < Content-Type: application/json; charset=utf-8
    {"GET":{"description":"Retreive a list of all customers."},"POST":{"description":"Create a new customer."}}

