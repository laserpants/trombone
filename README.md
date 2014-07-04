trombone2
=========

-- under construction --

### Arrow symbols (SQL route types)

     --   An SQL statement which does not return a result. 
     >>   A query of a type that returns a collection.
     ~>   A query that returns a single item.
     ->   Same as ~> except that an 'Ok' status message is added to the result.
     <>   An INSERT statement that should return a 'last insert id'. 
     ><   A statement that returns a row count result.
    
### Non-SQL route symbols
    
     ||   A request pipeline.
    <js>  A nodejs route.
    {..}  A static route. Followed by a JSON object. 

