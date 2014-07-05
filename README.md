trombone2
=========

-- under construction --

### Introduction

Trombone is a JSON-server that facilitates RESTful single-point data access and uses PostgreSQL as storage backend. Its purpose is to map HTTP requests to preconfigured SQL templates. These templates are instantiated and executed against a database, with results returned in JSON, using standard HTTP response codes.

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

| Symbol | Explanation
| ------ | -----------
| &dash;&dash; | An SQL statement which does not return a result. 
| >>     | A query of a type that returns a collection.
| ~>     | A query that returns a single item.
| ->     | Same as `~>` except that an 'Ok' status message is added to the result.
| <>     | An `INSERT` statement that should return a 'last insert id'.
| ><     | A statement that returns a row count result.

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

**todo**: The "Allow" HTTP header should be set when responding to OPTIONS requests. Perhaps "extract" this value from the JSON object, e.g.:

    OPTIONS /photo  {..}  {"<Allow>":"GET,POST,OPTIONS","GET":{"description":"Retreive a list of all photos."},"POST":{"description":"Create a new photo."}}
  
#### Response codes

### Command line flags

### Conventions

#### PATCH is your friend

#### OPTIONS could be your friend

### Authentication

### Middleware

### Pipelines

### Node.js integration
