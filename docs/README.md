Trombone
========

Trombone is a JSON-driven data access server for PostgreSQL. Its chief purpose is to translate JSON-formatted HTTP requests to SQL statemets. 

You may find this software useful if;

1. your application infrastructure relies on relational SQL schemas; and
2. you need to expose JSON data through a RESTful web service.

A Trombone configuration file consists of a collection of route patterns:

```
GET resource/:id  ->  SELECT * FROM some_table WHERE id = {{:id}}
```

- Source Code: github.com/johanneshilden/trombone

License
-------

The project is licensed under the BSD license.
