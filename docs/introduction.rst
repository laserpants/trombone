Introduction
============

Trombone facilitates effortless adaptation of conventional SQL schemas to mobile-friendly data exchange, based on JSON and the RESTful web service paradigm. It uses PostgreSQL as underlying RDBMS and translates JSON-formatted requests to database statements, according to rules outlined by a collection of pre-configured route templates, such as the one shown here.

::

    GET resource/:id   ->   SELECT * FROM stuff WHERE id = {{:id}}


The route format is `described in more detail here <route-format.html>`_.


Hello, World!
-------------

@todo
