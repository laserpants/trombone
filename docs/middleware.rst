Middleware
==========

.. 

Middlewares are built-in, auxiliary software components providing some functionality which is normally disabled (with the exception of file serving). These components may be enabled at run-time and configured to suit specific needs. See respective section for details on how to activate and configure a component.

Available Components
--------------------

* `RabbitMQ`_
* `CORS`_ (cross-origin resource sharing)
* `Logging`_
* `Static File Serving`_

RabbitMQ
--------

RabbitMQ is a a messaging system based on the Advanced Message Queuing Protocol -- an emerging standard for multi-purpose, asynchronous message delivery. The AMQP middleware integrates Trombone with RabbitMQ and makes it possible for participating applications to receive notifications when server resources are modified.

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

When a request of type ``POST``, ``PUT``, ``DELETE``, or ``PATCH`` is accepted and produces a regular ``200 OK`` response, a subsequent message is published to an exchange managed by the server.

Trombone AMQP Exchange
``````````````````````

======== =========================
Name     ``exchange/trombone/api``
Type     ``fanout``
======== =========================

Messages follow the format ``<method> <uri>:<response-body>``; e.g.,

::

    POST customer/new:{"status":true,"id":49,"message":"Ok."}


Using AMQP in JavaScript applications
*************************************

To configure and run RabbitMQ with STOMP Over WebSocket enabled, follow `these instructions <http://www.rabbitmq.com/web-stomp.html>`_ to install the Web-Stomp plugin.

| *STOMP is a simple text-orientated messaging protocol. It defines an interoperable wire format so that any of the available STOMP clients can communicate with any STOMP message broker to provide easy and widespread messaging interoperability among languages and platforms.*

For more information on STOMP Over WebSocket, see http://jmesnil.net/stomp-websocket/doc/.

JavaScript Example
``````````````````

For this example, you need stomp.js, and sock.js.

* http://jmesnil.net/stomp-websocket/doc/#download
* http://cdn.sockjs.org/sockjs-0.3.min.js


.. sourcecode:: html

    <!DOCTYPE html>
    <html lang="en">
        <head>
            <meta charset="utf-8">
            <title>Trombone/RabbitMQ over STOMP</title>
        </head>
        <body>
    
            <div id="notification"></div>
    
            <script type="text/javascript" src="js/sockjs.min.js"></script>
            <script type="text/javascript" src="js/stomp.min.js"></script>
            <script type="text/javascript">
    
                // See: http://www.rabbitmq.com/web-stomp.html
                var ws = new SockJS('http://127.0.0.1:55674/stomp'),
                    client = Stomp.over(ws);
    
                // Heartbeats won't work with SockJS.
                client.heartbeat.outgoing = 0;
                client.heartbeat.incoming = 0;
    
                var onConnect = function() {
                    client.subscribe('/exchange/trombone/api', function(msg) {
                        var div = document.getElementById('notification');
                        div.innerHTML += msg.body + '<br>';
                    });
                };
    
                var onError = function() {
                    console.log('Error connecting to RabbitMQ server.');
                };
    
                client.connect('guest', 'guest', onConnect, onError, '/');
    
            </script>
        </body>
    </html>


CORS
----

The CORS component provisions Trombone with the ability to accept cross-domain requests. It implements the handshake and response headers mandated by CORS-compliant client applications, such as modern web browsers. 

| *CORS introduces a standard mechanism that can be used by all browsers for implementing cross-domain requests. The spec defines a set of headers that allow the browser and server to communicate about which requests are (and are not) allowed. CORS continues the spirit of the open web by bringing API access to all.*

.. NOTE::

    CORS involves coordination between both server and client. For more information regarding client requirements, as well as cross-origin resource sharing in general, please see: `enable-cors.org <http://enable-cors.org>`_.


+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Flags                                                                                                                                                                       |
+=============================================================================================================================================================================+
| Enable using ``--cors`` or ``-C``.                                                                                                                                          |
+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

Logging
-------

The logging format is similar to Apache's log file output. 

.. role:: raw-html(raw)
   :format: html

+----------------------------------------------------------------------------------------------------------------+
| Flags                                                                                                          |
+================================================================================================================+
| Enable using ``--access-log[=FILE]`` or ``-l``, and specify ``--colors`` to enable :raw-html:`<br />`          |
| colors in the log file.                                                                                        |
+----------------------------------------------------------------------------------------------------------------+

Typical output
**************

@todo

Static File Serving
-------------------

Trombone can also act as a simple file server. Files located under the ``public/`` directory or any of its subdirectories are HTTP accessible. 

::

    public/image.png   <~>   http://localhost:3010/image.png

