Trombone
========

### Introduction

Trombone is a JSON-server that facilitates RESTful single-point data access. Using PostgreSQL as storage backend, its purpose is to map HTTP requests to preconfigured SQL templates. These templates are instantiated and executed against a database, with results returned in JSON, using standard HTTP response codes and error conventions.

### Hello, world!

`routes.conf:`

    GET    photo              >>  select * from photo order by id
    GET    photo/:id          ->  select * from photo where id = {{:id}}
    POST   photo              <>  insert into photo (url, description) values ('{{url}}', '{{description}}')
    PUT    photo/:id          ><
    PATCH  photo/:id          --
    DELETE photo              --

    GET    comment/photo/:id

### Configuration

#### Route format

##### Comments

##### BNF grammar

#### Types of routes

##### Database routes

| Symbol   | Explanation
| -------- | -----------
| `--`     | An SQL statement which does not return any result. 
| `>>`     | A query of a type that returns a collection.
| `~>`     | A query that returns a single item.
| `->`     | Same as `~>` except that an 'Ok' status message is added to the result.
| `<>`     | An `INSERT` statement that should return a 'last insert id'.
| `><`     | A statement that returns a row count result.

##### Non-SQL routes

| Symbol | Explanation
| ------ | -----------
|  &#124;&#124; | A request pipeline. (Followed by a pipeline name.)
| &lt;js&gt;    | A nodejs route. (Followed by a  file path to the script.)
| {..}          | A static route. (Followed by a JSON object.) 

##### Parameter hints

##### SELECT * FROM

##### Static routes

A possible use case for static routes is to provide documentation of a web service, using the `OPTIONS` HTTP method.

    OPTIONS /photo  {..}  {"GET":{"description":"Retreive a list of all photos."},"POST":{"description":"Create a new photo."}}
  
#### Response codes

### Command line flags

| Flag |             | Description
| ---- | ----------- | --------------------------------------------
| `-V` | `--version` | display version number and exit

#### Default values

| Option | Default value  
| ------ | --------- 
| x      | todo

### Conventions

#### PATCH is your friend

> The HTTP method PATCH can be used to update partial resources. For instance, when you only need to update one field of the resource, PUTting a complete resource representation might be cumbersome and utilizes more bandwidth. See more at: http://restcookbook.com/HTTP%20Methods/patch/#sthash.14B7n34z.dpuf


#### OPTIONS could be your friend

> This method allows the client to determine the options and/or requirements associated with a resource, or the capabilities of a server, without implying a resource action or initiating a resource retrieval.

Static JSON response routes support a special `<Allow>` keyword which can be used for this purpose: 

    OPTIONS /photo  {..}  {"<Allow>":"GET,POST,OPTIONS","GET":{"description":"Retreive a list of all photos."},"POST":{"description":"Create a new photo."}}

A typical response will then be:

    < HTTP/1.1 200
    < Allow: 'GET,POST,OPTIONS'
    < Content-Type: application/json; charset=utf-8
    {"GET":{"description":"Retreive a list of all customers."},"POST":{"description":"Create a new customer."}}


### Authentication

### Middleware

#### AMQP

#### CORS

#### Logging

### Pipelines

### Node.js integration
