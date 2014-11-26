Route Format
============

A Trombone configuration file consists of a collection of route patterns. The format of a single route is described by the following (high-level) grammar.

::

    <route> ::= <method> <uri> <symbol> <action>

For a more thorough description of this syntax, please see `BNF grammar <bnf-grammar.html>`_.

A simple configuration file is given below.

::

    # Return all customers
    GET  /customer      >>  SELECT * FROM customers

    # Return a single customer, or a 404 error
    GET  /customer/:id  ->  SELECT * FROM customers WHERE id = {{:id}}

    # Create a new customer
    POST /customer      <>  
    
        INSERT INTO customers 
            ( name
            , phone
            , industry ) 
        VALUES 
            ( {{name}}
            , {{phone}}
            , {{industry}} )


During dispatch, the server scans the list of routes for a possible match, based on the uri components and HTTP method used in the request.

The arrow symbol specifies the type of route and the response object's expected format. See `below <#types-of-routes>`_ for explanations of these symbols. The particular arrow used here; ``->``, denotes an SQL query with a singleton result.

Placeholders
------------

Placeholders are indicated by a double pair of surrounding curly-braces (akin to Handlebars.js). Trombone templates acknowledge three types of placeholder variables:

* Uri segment ``{{:variables}}``;
* JSON value ``{{placeholders}}``; and
* DRY-block placeholders ``{{..}}``.

Uri variables
*************

Request body JSON values
************************

DRY-block placeholders
**********************

DRY-block notation is explained under `DRY-block Notation <dry-block-notation.html>`_.

Types of Routes
---------------

Database routes
***************

============ =================================================================================
Symbol       Explanation
------------ ---------------------------------------------------------------------------------
``--``       An SQL statement that does not return any result. 
``>>``       A query of a type that returns a collection.
``~>``       A query that returns a single item.
``->``       Identical to ``~>`` except that an 'Ok' status message is added to the result.
``<>``       An ``INSERT`` statement that should return a 'last insert id'.
``><``       A statement that returns a row count result (e.g. ``UPDATE``).
============ =================================================================================

