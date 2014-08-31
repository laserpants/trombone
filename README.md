Trombone
========

-- under construction --

### Introduction

Trombone is a [REST](http://en.wikipedia.org/wiki/Representational_state_transfer)ful, [JSON](http://json.org/)-driven [data access server](http://en.wikipedia.org/wiki/Data_access_layer) for [PostgreSQL](www.postgresql.org). Its purpose is to map HTTP requests to preconfigured SQL templates. These templates are instantiated and executed against a database, with results returned in JSON, using   standard HTTP response codes and error conventions.

A Trombone configuration file consists of a collection of route patterns.

    GET resource/:id  ->  select * from some_table where id = {{:id}}

The format of a single route is described by the following (high-level) grammar. 

    <route> ::= <method> <uri> <symbol> <action>

During dispatch, the server will look through the list of routes for a possible match, based on the request's uri components and the HTTP method used. 

The arrow symbol specifies the type of route and the response object's expected format. See below for an explanation of these symbols. The particular arrow used here; `->`, denotes an SQL query with a singleton result.

### Hello, world!

`routes.conf:`

**todo**

    GET    photo              >>  select * from photo order by id
    GET    photo/:id          ->  select * from photo where id = {{:id}}
    POST   photo              <>  insert into photo (url, description) values ('{{url}}', '{{description}}')
    PUT    photo/:id          ><
    PATCH  photo/:id          ><
    DELETE photo              --

    GET    comment/photo/:id

### Configuration

#### Route format

##### Placeholders

Trombone templates acknowledge two types of placeholder variables, both denoted by a double pair of surrounding  curly-braces (mimicking Handlebars.js):

* Uri segment `{{:variables}}` and
* JSON value `{{placeholders}}`.

###### Uri variables

    GET customer/:id   ->   select * from customer where id = {{:id}}

This type of variable must also be present in the route's uri pattern, where it is bound to a specific path segment. A uri variable can only contain alphanumeric characters, hyphens and underscores. It is always prefixed with a single colon to make the distinction clear from ordinary request body placeholders, which are explained next.    

###### Request body JSON values

    POST /customer  <>  insert into customer (name, address, phone) values ({{name}}, {{address}}, {{phone}})

When a JSON-formatted request body is available, the server will first parse the raw body to JSON  and substitute any placeholders in the template with matching values in the JSON object whose keys correspond to the names of the placeholders in question. 

```
{
    "name": "OCP",
    "address": "Delta City",
    "phone": "555-MEGACORP"
}
```

...

    insert into customer (name, address, phone) values ('OCP', 'Delta City', '555-MEGACORP')

> Use the `--verbose` command-line option to inspect the query string after a template is instantiated.

##### Gotcha!

Note that all SQL keywords (`select`, `from`, `insert`, `into` etc.) must be written in lowercase!

##### Comments

Comments start with a single [octothorpe](http://en.wikipedia.org/wiki/Number_sign) (#) character and may appear at the end of a route definition;

    GET photo       >>  select * from photo   # Retreive all photos.

or stretch over an entire line; 

    # Return a specific photo.
    GET photo/:id   ->  select * from photo where id = {{:id}}

##### Multi-line expressions

SQL statements are allowed to span across multiple lines, as long as each subsequent, non-empty line is indented with, at least, one blank space; as in the example below.


```
GET resource  >>  

      select name,           
             address,        
             phone,          
             shoe_size       
      from customer          
      order by id
```

This, however, is not valid:

```
GET resource  >>  

select name,           
       address,        
       phone,          
       shoe_size       
from customer          
order by id
```

Except from the "single-space" requirement, indentation does not matter. Hence, the following is also valid.

```
GET resource  >>  select name           
                       , address        
                       , phone          
                       , shoe_size       
                  from customer          
                  order by 
                    id
```
    
##### BNF grammar

#### Types of routes

##### Database routes

| Symbol   | Explanation
| -------- | -----------
| `--`     | An SQL statement that does not return any result. 
| `>>`     | A query of a type that returns a collection.
| `~>`     | A query that returns a single item.
| `->`     | Identical to `~>` except that an 'Ok' status message is added to the result.
| `<>`     | An `INSERT` statement that should return a 'last insert id'.
| `><`     | A statement that returns a row count result.

##### Naming conventions

Trombone assumes that database table and column names follow the usual `lowercase_separated_by_underscores`  convention and that JSON objects use `camelCase` formatting. Conversion between the two is implicit.

##### Arrays

...

##### SELECT * FROM

    select * from photo

##### Parameter hints

With joins, and more complex queries, the server can sometimes have some difficulty figuring out the attribute names to return. In such cases, and in situations where more control is needed, it is  therefore possible to explicitly specify a list of parameters. This list should appear immediately before the query template, enclosed in parentheses. 

###### Example

    GET /customer  >>  (id, name, phone) select a.a, a.b, a.c from customer as a join something as b

Incidentally, a similar syntax is available for `INSERT` statements. This can be used if the server is unable to infer the table name and sequence necessary to obtain the last inserted id.

    POST /customer  <>  (tbl_name, sequence) insert into...
   
##### Non-SQL routes

| Symbol | Explanation
| ------ | -----------
|  &#124;&#124; | A request pipeline. (Followed by a pipeline name.)
|  &#124;>      | An inline request pipeline. (Followed by a pipeline definition.)
| &lt;js&gt;    | A node.js route. (Followed by a  file path to the script.)
| {..}          | A static route. (Followed by a JSON object.) 

##### Pipelines

###### Inline

```
POST          !resource  |>
{
   "processors": [
      {
         "id": 1,
         "method": "POST",
         "uri": "resource",
         "fields": ["name"]
      }
   ],
   "connections": [
      {
         "destination": 1,
         "filters": [],
         "transformers": []
      },
      {
         "source": 1,
         "filters": [],
         "transformers": []
      }
   }
}
```

##### Node.js

##### Static routes

A possible use-case for static routes is to provide documentation as part of your web service, typically using the `OPTIONS` HTTP method.

    OPTIONS /photo  {..}  {"GET":{"description":"Retreive a list of all photos."},"POST":{"description":"Create a new photo."}}

##### A note about wildcard operators

Since string values are always wrapped in single quotation marks before inserted into a template, the following will not work as intended,

```
select * from customer where customer.name like ('%{{q}}%')
```

Instead, define your template as

```
select * from customer where customer.name like ({{q}})

```

and insert the `%`-characters in a string property of the object sent to the server:

```
{
   "q": "%Mega-corp%"
}
```

#### Response codes

### Running the server

#### Ping

    http://localhost:3000/ping

...

```
< HTTP/1.1 200 
< Transfer-Encoding: chunked
< Content-Type: application/json; charset=utf-8
< Server: Trombone/0.8
{
   "status":true,
   "message":"Pong!"
}
```

#### Command line flags

| Flag | Long option      | Description
| ---- | ---------------- | --------------------------------------------
| `-V` | `--version`      | display version number and exit
| `-?` | `--help`         | display this help and exit
| `-x` | `--disable-hmac` | disable message integrity authentication (HMAC)
| `-C` | `--cors`         | enable support for cross-origin resource sharing
| `-A[USER:PASS]` | `--amqp[=USER:PASS]` | enable RabbitMQ messaging middleware [username:password]
| `-i[FILE]` | `--pipelines[=FILE]` | enable request pipelines [configuration file]
| `-s PORT`  | `--port=PORT`        | server port
| `-l[FILE]` | `--access-log[=FILE]` | enable logging to file [log file]
|            | `--size=SIZE`         | log file size
| `-h HOST`  | `--db-host=HOST`      | database host
| `-d DB`    | `--db-name=DB`       | database name
| `-u USER`  | `--db-user=USER`     | database user
| `-p PASS`  | `--db-password=PASS` | database password
| `-P PORT`  | `--db-port=PORT`     | database port
| `-r FILE`  | `--routes-file=FILE` | route pattern configuration file
| `-t`       | `--trust-localhost`  | skip HMAC authentication for requests from localhost
|            | `--pool-size=SIZE`   | number of connections to keep in PostgreSQL connection pool
|            | `--verbose`          | print various debug information to stdout

#### Default values

Many of these settings have sensible default values:

| Option        | Default value  
| ------------- | --------- 
| AMQP user 	  | "guest"
| AMQP password | "guest"
| Server port   | 3010
| Log file 	  | "log/access.log"
| Log size 	  | 4,096 bytes
| DB-host       | "localhost"
| DB-name       | "trombone"
| DB-user       | "postgres"
| DB-password   | "postgres"
| DB-port       |	5432
| Routes file   | "routes.conf"
| Pipelines file | "pipelines.conf"
| Pool size 	  | 10

### REST Conventions

#### PATCH is your friend

> The HTTP method PATCH can be used to update partial resources. For instance, when you only need to update one field of the resource, PUTting a complete resource representation might be cumbersome and utilizes more bandwidth. See more at: http://restcookbook.com/HTTP%20Methods/patch/#sthash.14B7n34z.dpuf


#### OPTIONS could be your friend

> This method allows the client to determine the options and/or requirements associated with a resource, or the capabilities of a server, without implying a resource action or initiating a resource retrieval.

Static JSON response routes support a special `<Allow>` keyword for this purpose: 

    OPTIONS /photo  {..}  {"<Allow>":"GET,POST,OPTIONS","GET":{"description":"Retreive a list of all photos."},"POST":{"description":"Create a new photo."}}

A typical response will then be:

    < HTTP/1.1 200
    < Allow: 'GET,POST,OPTIONS'
    < Content-Type: application/json; charset=utf-8
    {"GET":{"description":"Retreive a list of all customers."},"POST":{"description":"Create a new customer."}}

#### To DELETE a non-existing resource is 200 OK

> The DELETE method is idempotent. This implies that the server must return response code 200 (OK) even if the server deleted the resource in a previous request. (http://shop.oreilly.com/product/9780596801694.do)

##### Idempotency in a nutshell

#### GET should not depend on the request body

> The GET method means retrieve whatever information (in the form of an entity) is identified by the Request-URI.

### Authentication

To establish the authenticity of a request, the server performs a message integrity check, using a cryptographic primitive known as a HMAC (hash-based message authentication code). A MAC code is attached to the request in the form of an `API-Access` header. A second code is computed from the request object using a token associated with the client application. The result of this operation is then compared to the MAC attached to the request in order to verify its authenticity.

##### Table schema

    CREATE TABLE trombone_keys (
        id serial,
        client character varying(40),
        key character varying(40)
    );

    ALTER TABLE ONLY trombone_keys
        ADD CONSTRAINT trombone_keys PRIMARY KEY (id);

    API-Access: client:hash

Authentication is enabled by default. A client application that wishes to consume the service must therefore,

1. be present in the `trombone_keys` database table with a unique identifier and the secure token; as well as
* supply the following HTTP header with each request:

```
API-Access: client:hash
```

where the `hash` is a MAC code generated by combining the request body with aforementioned key, using the HMAC-SHA1 algorithm.


##### JavaScript implementation

##### Disable HMAC authentication

Message authentication can be disabled using the `-x` command line switch. Doing so is not recommended in a production environment, since it renders the system vulnerable to unauthorized access.

To circumvent HMAC authentication specifically for requests originating from the local host, instead use the `-t`, or `--trust-localhost` option. 

### Middleware

Component | Flag
--------- | --------
AMQP 	   | `--amqp[=USER:PASS]` or `-A`
CORS 	   | `--cors` or `-C`
Logging   | `--access-log[=FILE]` or `-l`
File server | (always enabled)

#### AMQP

RabbitMQ is a a messaging system based on the Advanced Message Queuing Protocol (AMQP) — an emerging standard for asynchronous message passing. The AMQP middleware integrates Trombone with RabbitMQ, and allows consumer applications to receive notifications when server resources are modified.

##### AMQP endpoint

    /exchange/trombone/api

##### Using AMQP in JavaScript applications

#### CORS

The CORS component provides support for cross-domain requests.

> JavaScript and the web programming has grown by leaps and bounds over the years, but the same-origin policy still remains. This prevents JavaScript from making requests across domain boundaries, and has spawned various hacks for making cross-domain requests.

> CORS introduces a standard mechanism that can be used by all browsers for implementing cross-domain requests. The spec defines a set of headers that allow the browser and server to communicate about which requests are (and are not) allowed. CORS continues the spirit of the open web by bringing API access to all.

For more information about cross-origin resource sharing, please see: http://enable-cors.org.

To enable CORS support, use the `-C` or `--cors` command line option.

#### Logging

Apache-style logging.

#### Role-based authorization

### Pipelines

### Node.js integration

### Static file serving

Trombone can also act as a simple file server. Files located under the `public` directory or any of its subdirectories, are HTTP accessible to everyone. E.g.,

    public/image.png   <~>   http://localhost:3000/image.png

### About Trombone

Trombone is written in Haskell using the Warp HTTP server library, and WAI — a common protocol for communication between web applications and web servers. 

This software library is available for use under the BSD license.
