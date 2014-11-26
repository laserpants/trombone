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

RabbitMQ is a a messaging system based on the Advanced Message Queuing Protocol -- an emerging standard for multi-purpose, asynchronous message passing. The AMQP middleware integrates Trombone with RabbitMQ and facilitates for consumer applications to receive notifications when server resources are modified.

+-------------------------------------------------------------------------------------------+
| Flags                                                                                     |
+===========================================================================================+
| Enable with ``--amqp[=USER:PASS]`` or ``-A`` and, optionally, supply a host name using    |
| ``--amqp-host[=HOST]`` (if you leave out this option, ``localhost`` is assumed).          |
+-------------------------------------------------------------------------------------------+

AMQP Endpoint
*************

When a request of type ``POST``, ``PUT``, ``DELETE``, or ``PATCH`` is accepted, a message of the format ``<method> <uri>:<response-body>`` is published to an exchange with the following settings:

======== =========================
Name     ``exchange/trombone/api``
Type     ``fanout``
======== =========================

Example:

::

    POST customer/new:{"status":true,"id":49,"message":"Ok."}


Using AMQP in JavaScript applications
*************************************

To configure and run RabbitMQ with STOMP Over WebSocket enabled, follow the instructions to install the `Web-Stomp plugin <http://www.rabbitmq.com/web-stomp.html>`_.

http://jmesnil.net/stomp-websocket/doc/

@todo

Example
```````

::

    <script type="text/javascript" src="http://cdn.sockjs.org/sockjs-0.3.min.js"></script>
    <script type="text/javascript" src="https://raw.githubusercontent.com/jmesnil/stomp-websocket/master/lib/stomp.min.js"></script>




CORS
----

The CORS component introduces the capability to accept cross-domain requests, by implementing the necessary hand-shaking and response headers. These are typically expected by client applications, such as modern web browsers, when sending CORS-enabled requests. 

| *CORS introduces a standard mechanism that can be used by all browsers for implementing cross-domain requests. The spec defines a set of headers that allow the browser and server to communicate about which requests are (and are not) allowed. CORS continues the spirit of the open web by bringing API access to all.*

.. NOTE::

    CORS involves coordination between both server and client. For more information regarding client requirements, and cross-origin resource sharing in general, please see: `enable-cors.org <http://enable-cors.org>`_.


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

