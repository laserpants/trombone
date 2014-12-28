Route Format
============

A Trombone configuration file consists of a collection of route patterns. The format of a single route item is given by the following (high-level) grammar.

::

    <route> ::= <method> <uri> <symbol> <action>

For a more detailed description of the syntactic rules involved in this route schema, please see `BNF grammar <bnf-grammar.html>`_. What we consider here is a more general overview. 

As an example of a simple configuration file:

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


The server scans the list of routes during dispatch, carefully looking for a pattern that matches the uri components and HTTP method used in the request.

The arrow symbol specifies the type of route and the response object's expected format. See `below <#types-of-routes>`_ for explanations of these symbols. E.g., the particular arrow used here (``->``) denotes an SQL query with a singleton result.

Placeholders
------------

Placeholders are denoted by a double pair of surrounding curly-braces (akin to e.g., Handlebars.js). Trombone templates acknowledge three types of placeholder variables:

* JSON value ``{{placeholders}}``; 
* Uri segment ``{{:variables}}``; and
* DRY-block placeholders ``{{..}}``.

Request body JSON values
************************

When a JSON-formatted request body is present, the dispatch handler will first try to parse the object and substitute placeholders in the template with values whose keys corresponding to the names of the concerned variables. 

..  =======================  ==========================
    Route configuration:     ``POST /customer  <>  INSERT INTO customer (name, address, phone) VALUES ( {{name}}, {{address}}, {{phone}} )``              
    =======================  ==========================
    
    Request object:
    
    ::
    
        {
            "name": "OCP",
            "address": "Delta City",
            "phone": "555-MEGACORP"
        }
    
    
    =======================  ==========================
    Actual SQL query:        ``INSERT INTO customer (name, address, phone) VALUES ('OCP', 'Delta City', '555-MEGACORP')``
    =======================  ==========================


**Route configuration:**

::

    POST /customer  <>  INSERT INTO customer (name, address, phone) 
                        VALUES ( {{name}}, {{address}}, {{phone}} )


**Request object:**

::

    {
        "name": "OCP",
        "address": "Delta City",
        "phone": "555-MEGACORP"
    }


**Actual SQL query:**

::

    INSERT INTO customer (name, address, phone) 
    VALUES ('OCP', 'Delta City', '555-MEGACORP')


.. NOTE::

    Use the ``--verbose`` command-line option to inspect the final query string after a template is instantiated.


Uri variables
*************

Uri variables are simple placeholders that may conceal text or integer values, supplied as part of the request uri.

::

    GET customer/:id   ->   SELECT * FROM customer WHERE id = {{:id}}


Notice that the variable appears both in the query template (right-hand side of the arrow), and in the route's uri pattern, where it is bound to a specific path segment. The variable name must consist of only alphanumeric characters, hyphens and underscores. Furthermore, it is always prefixed with a single colon to make the distinction clear from ordinary request body placeholders. 


DRY-block placeholders
**********************

DRY-block notation is explained under `DRY-block Notation`_.

Comments
--------

Comments start with a single `octothorpe <http://en.wikipedia.org/wiki/Number_sign>`_ (``#``) character and may appear at the end of a route definition;

::

    GET photo       >>  SELECT * FROM photo   # Retreive all photos!


or stretch over an entire line; 

::

    # Return some specific photo.
    GET photo/:id   ->  SELECT * FROM photo WHERE id = {{:id}}


Multi-line expressions
----------------------

SQL routes are allowed to span across multiple lines, as long as each subsequent, non-empty line is indented with, at least, one blank space; as in the example below.

::

    GET resource  >>  
    
          select name,           
                 address,        
                 phone,          
                 shoe_size       
          from customer          
          order by id


This, however, is *not* valid:

::

    GET resource  >>  
    
    select name,           
           address,        
           phone,          
           shoe_size       
    from customer          
    order by id


Except from this "single-space" requirement, indentation does not matter. Hence, the following is a valid route description.

::

    GET resource  >>  select name           
                           , address        
                           , phone          
                           , shoe_size       
                      from customer          
                      order by 
                        id

Types of Routes
---------------

Database routes
***************

.. role:: raw-html(raw)
   :format: html

============ =====================================================================================
Symbol       Explanation
============ =====================================================================================
``--``       An SQL statement that does not return any result. 
``>>``       A query of a type that returns a collection.
``~>``       A query that returns a single item.
``->``       Identical to ``~>`` except that an 'Ok' status message is added to the :raw-html:`<br />` 
             JSON response.
``<>``       An ``INSERT`` statement that should return a 'last insert id'.
``><``       A statement that returns a row count result (e.g. ``UPDATE``).
============ =====================================================================================

Other routes
************

The following, additional route formats all share the common trait that they do not interact directly with the database.

============ =================================================================================
Symbol       Explanation
============ =================================================================================
``||``       A request pipeline. (Followed by a pipeline identifier.)
``|>``       An inline request pipeline. (Followed by a pipeline definition.)
``<js>``     A node.js route. (Followed by a file path to the script.)
``{..}``     A static route. (Followed by a JSON object.)
============ =================================================================================

These are `explained here <non-sql-routes.html>`_.


Parameter hints
---------------

With joins, and more complex queries, the server can have a difficult time figuring out the attribute names to return, from looking at the template alone. In such cases, and in situations where more control is needed, it is therefore possible (and necessary) to specify the list of property names. This list should appear immediately before the query template, enclosed in parentheses. 

::

    GET /customer  >>  
    
        (id, name, phone) 
        
        SELECT a.a, a.b, a.c 
        FROM customer 
          AS a 
        JOIN something 
          AS b...

A similar syntax is available for ``INSERT`` statements, which can be used if the server is unable to infer the table name and sequence necessary to obtain the last inserted id.

::

    POST /customer  <>  (tbl_name, sequence) INSERT INTO...


Special Considerations
----------------------

SELECT * FROM
*************

``SELECT * FROM``-type of queries are accepted as a convenient shorthand. The server will attempt to expand the column names during preprocessing of the configuration file. However, this is not guaranteed to work. In some cases you will have to explicitly write out the column names, e.g., ``SELECT id, name, favorite_cheese FROM...``.


Wildcard operators
******************

Since string values are automatically wrapped in single quoutes before they are inserted into a template, the following will not work as expected,

::

    SELECT * FROM customer WHERE customer.name LIKE '%{{q}}%'


E.g., ``{"q": "ACME"}`` would translate to ``customer.name LIKE '%'ACME'%'``.

This is clearly not what we intended. Instead, define your template as

::

    SELECT * FROM customer WHERE customer.name LIKE {{q}}


and insert the ``%``-characters inside the string property of the object sent to the server:

::

    {
       "q": "%ACME%"
    }


DRY-block Notation
------------------

A common pattern is to have multiple database queries that are similar in one way or another.

::

    GET customer/all        >>
       select id, name, phone, address from customer order by id
    
    GET customer/:id        ->
       select id, name, phone, address from customer where id = {{:id}}
    
    GET customer/area/:id   >>
       select id, name, phone, address from customer where area_id = {{:id}} order by id


To avoid repetition, an alternative `DRY <http://en.wikipedia.org/wiki/Don%27t_repeat_yourself>`_ notation can be employed in cases such as this. The following is an equivalent route definition using a DRY-block.

::

    DRY
         select id, name, phone, address from customer {{..}}      # base template
    {
         GET customer/all       >>  order by id                          ;
         GET customer/:id       ->  where id = {{:id}}                   ;
         GET customer/area/:id  >>  where area_id = {{:id}} order by id  
    }


A DRY-block consists of a *base template* and a number of *stubs*, each with the segment of the statement unique to its corresponding route.

::

    <method> <uri> <symbol> <stub>

Here are some important observations.

*   The ``{{..}}``-placeholder must appear in the base query to indicate where the stub should be inserted. The preprocessor looks at each item within the block, expands it by inserting the base query with the stub replaced for ``{{..}}``.

*   A semi-colon delimiter is required to separate the stubs within the block. (It may be omitted for the last item.)

*   Each block item must be indented with at least one blank space. The opening and closing brackets should appear on their own lines (without indentation):

::

    {
        GET /..
        GET /..
    }


