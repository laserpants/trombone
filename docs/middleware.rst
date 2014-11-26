Middleware
==========

Middlewares are server components which provide some additional functionality. They can be enabled at run-time and configured to suit specific needs. With exception of file serving, middlewares are disabled by default. See respective section for details on how configuration works for a specific component.

Available Components
--------------------

* `RabbitMQ`_
* `CORS`_ (cross-origin resource sharing)
* `Logging`_
* `Static File Serving`_

RabbitMQ
--------

RabbitMQ is a a messaging system based on the Advanced Message Queuing Protocol -- an emerging standard for multi-purpose asynchronous message passing. The AMQP middleware integrates Trombone with RabbitMQ and facilitates for consumer applications to receive notifications when server resources are modified.

AMQP Endpoint
*************

The following AMQP exchange should be specified when connecting to the service: ``/exchange/trombone/api``.

+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Flags                                                                                                                                                                       |
+=============================================================================================================================================================================+
| Enable with ``--amqp[=USER:PASS]`` or ``-A`` and, optionally, supply a host name using ``--amqp-host[=HOST]`` (if you leave out this option, ``localhost`` is assumed).     |
+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

Examples of using AMQP in JavaScript applications
*************************************************

@todo

CORS
----

The CORS component introduces the capability to accept cross-domain requests, by implementing the necessary hand-shaking and response headers. These are typically expected by client applications, such as modern web browsers, when sending CORS-enabled requests. 

| *JavaScript and the web programming has grown by leaps and bounds over the years, but the same-origin policy still remains. This prevents JavaScript from making requests across domain boundaries, and has spawned various hacks for making cross-domain requests.*
|
| *CORS introduces a standard mechanism that can be used by all browsers for implementing cross-domain requests. The spec defines a set of headers that allow the browser and server to communicate about which requests are (and are not) allowed. CORS continues the spirit of the open web by bringing API access to all.*

.. NOTE::

    For more information about cross-origin resource sharing, please see: `enable-cors.org <http://enable-cors.org>`_.


+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Flags                                                                                                                                                                       |
+=============================================================================================================================================================================+
| Enable using ``--cors`` or ``-C``.                                                                                                                                          |
+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

Logging
-------

@todo

+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Flags                                                                                                                                                                       |
+=============================================================================================================================================================================+
| ``--access-log[=FILE]`` or ``-l``                                                                                                                                           |
+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

Static File Serving
-------------------

Trombone can also act as a simple file server. By convention, files located under the ``public/`` directory or any of its subdirectories are HTTP accessible.

