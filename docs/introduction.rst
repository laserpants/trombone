Introduction
============

Trombone facilitates effortless adaptation of conventional SQL schemas to mobile-friendly APIs operating within the RESTful web service paradigm. It uses PostgreSQL as underlying RDBMS and translates JSON-formatted requests to database statements, according to rules layed out by a set of route templates, such as the one below.

.. data exchange

::

    GET resource/:id   ->   SELECT * FROM stuff WHERE id = {{:id}}


This format is `described in more detail here <route-format.html>`_.


Hello, World!
-------------

@todo
