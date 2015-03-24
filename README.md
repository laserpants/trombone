Trombone
========

Trombone is a data access server for PostgreSQL. Its chief purpose is to translate JSON-formatted HTTP requests to database statements with results returned in JSON, while complying with standard response code and error conventions. 

You may find this software useful if;

1. your application relies on relational SQL schemas; and
2. you need to expose JSON data via a RESTful web service interface.

#### In a nutshell

A Trombone configuration file consists of a collection of route patterns:

```
GET resource/:id  ->  SELECT * FROM resources WHERE id = {{:id}}
```

Documentation
-------------

- http://trombone.readthedocs.org

Contribute
----------

- Issue Tracker: [https://github.com/johanneshilden/trombone/issues](https://github.com/johanneshilden/trombone/issues)
- Source Code: [https://github.com/johanneshilden/trombone](https://github.com/johanneshilden/trombone)


License
-------

The project is licensed under the BSD license.
