Trombone
========

Trombone is a data access server for PostgreSQL. Its chief purpose is to translate JSON-formatted HTTP requests to SQL statemets, with results returned in JSON using standard HTTP response codes and error conventions. 

You may find this software useful if;

1. your application infrastructure relies on relational SQL schemas; and
2. you need to expose JSON data through a RESTful web service.

A Trombone configuration file consists of a collection of route patterns:

```
GET resource/:id  ->  SELECT * FROM some_table WHERE id = {{:id}}
```

Documentation
-------------

- http://trombone.readthedocs.org

Contribute
----------

- Issue Tracker: [github.com/johanneshilden/trombone/issues](github.com/johanneshilden/trombone/issues)
- Source Code: [github.com/johanneshilden/trombone](github.com/johanneshilden/trombone)


License
-------

The project is licensed under the BSD license.
