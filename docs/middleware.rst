Middleware
==========

.. Middlewares are built-in software components which provide some auxiliary functionality and may be configured to suit specific needs. With the exception of file serving, middlewares are disabled by default. See respective section for details on how to activate and configure a specific component.

Middlewares are built-in software components providing some auxiliary functionality, which is normally disabled (with the exception of file serving). These components may be enabled at run-time and configured to suit specific needs. See respective section for details on how to activate and configure a specific component.

Available Components
--------------------

* `RabbitMQ`_
* `CORS`_ (cross-origin resource sharing)
* `Logging`_
* `Static File Serving`_

RabbitMQ
--------

RabbitMQ is a a messaging system based on the Advanced Message Queuing Protocol -- an emerging standard for multi-purpose, asynchronous message passing. The AMQP middleware integrates Trombone with RabbitMQ and makes it possible for participating applications to receive notifications when server resources are modified.

.. role:: raw-html(raw)
   :format: html

+-----------------------------------------------------------------------------------------------------------+
| Flags                                                                                                     |
+===========================================================================================================+
| Enable with ``--amqp[=USER:PASS]`` or ``-A`` and, optionally, supply a host name using :raw-html:`<br />` |
| ``--amqp-host[=HOST]`` (if you leave out this option, ``localhost`` is assumed).                          |
+-----------------------------------------------------------------------------------------------------------+

AMQP Endpoint
*************

When a request of type ``POST``, ``PUT``, ``DELETE``, or ``PATCH`` is accepted and produces an ordinary ``200 OK`` response, a message is published to an exchange with the following attributes:

======== =========================
Name     ``exchange/trombone/api``
Type     ``fanout``
======== =========================

Messages follow the format ``<method> <uri>:<response-body>``; e.g.,

::

    POST customer/new:{"status":true,"id":49,"message":"Ok."}


Using AMQP in JavaScript applications
*************************************

To configure and run RabbitMQ with STOMP Over WebSocket enabled, follow the instructions to install the `Web-Stomp plugin <http://www.rabbitmq.com/web-stomp.html>`_.

http://jmesnil.net/stomp-websocket/doc/

@todo

Example
```````

.. sourcecode:: html

    <script type="text/javascript" src="http://cdn.sockjs.org/sockjs-0.3.min.js"></script>
    <script type="text/javascript" src="https://raw.githubusercontent.com/jmesnil/stomp-websocket/master/lib/stomp.min.js"></script>




CORS
----

The CORS component provisions Trombone with the ability to accept cross-domain requests. It implements the handshake and response headers mandated by CORS-compliant client applications, such as modern web browsers. 

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

+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Flags                                                                                                                                                                       |
+=============================================================================================================================================================================+
| Enable using ``--access-log[=FILE]`` or ``-l``, and specify ``--colors`` to enable colors in the log file.                                                                  |
+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

Typical log output
******************

@todo

Static File Serving
-------------------

Trombone can also act as a simple file server. Files located under the ``public/`` directory or any of its subdirectories are HTTP accessible. E.g.,

::

    public/image.png   <~>   http://localhost:3010/image.png

