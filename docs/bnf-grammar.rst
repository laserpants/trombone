BNF Grammar
===========

@todo

::

    <route>     ::= <method> <uri> <action>

    <method>    ::= "GET" | "POST" | "PUT" | "PATCH" | "DELETE" | "OPTIONS"

    <uri>       ::= [ <delim> ] { <item> <delim> }

    <delim>     ::= "/"

    <item>      ::= <variable> | <atom>

    <variable>  ::= 

    <atom>      ::= 

    <action>    ::= <sql-route> 
                  | <pipeline-route> 
                  | <inline-route> 
                  | <static-route> 
                  | <node-js-route>

    <sql-route> ::= <sql-no-result> 
                  | <sql-item>
                  | <sql-item-ok>
                  | <sql-collection>
                  | <sql-last-insert>
                  | <sql-count>

    <sql-no-result>   ::= "--"
    <sql-item>        ::= "~>"
    <sql-item-ok>     ::= "->"
    <sql-collection>  ::= ">>"
    <sql-last-insert> ::= "<>"
    <sql-count>       ::= "><"

    <pipeline-route>  ::= "||"

    <inline-route>    ::= "|>"

    <static-route>    ::= "{..}"

    <node-js-route>   ::= "<js>"



.. NOTE::
    A complete treatment of the SQL syntax is beyond the scope of this document.

