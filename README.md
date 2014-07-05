trombone2
=========

-- under construction --

### Introduction

### Hello, world!

### REST

### JSON

### Server configuration

#### Route format

##### BNF grammar

#### Types of routes

##### Database routes

| Symbol | Explanation
| ------ | -----------
| `--`   | An SQL statement which does not return a result. 
| `>>`   | A query of a type that returns a collection.
| `~>`   | A query that returns a single item.
| `->`   | Same as `~>` except that an 'Ok' status message is added to the result.
| `<>`   | An `INSERT` statement that should return a 'last insert id'.
| `><`   | A statement that returns a row count result.

##### Non-SQL routes

| Symbol | Explanation
| ------ | -----------
|        | A request pipeline. (Followed by a pipeline name.)
| `<js>` | A nodejs route. (Followed by a relative file path to the script.)
| `{..}` | A static route. (Followed by a JSON object.) 
  
#### Response codes

### Authentication

### Middleware

### Pipelines

### Node.js integration
