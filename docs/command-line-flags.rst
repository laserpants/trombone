Command Line Flags
==================

==================== ======================= ==============================================================
Flag                 Long option             Description
==================== ======================= ==============================================================
  ``-V``             ``--version``           display version number and exit
  ``-?``             ``--help``              display this help and exit
  ``-x``             ``--disable-hmac``      disable message integrity authentication (HMAC)
  ``-C``             ``--cors``              enable support for cross-origin resource sharing
  ``-A[USER:PASS]``  ``--amqp[=USER:PASS]``  enable RabbitMQ messaging middleware [username:password]
  ..                 ``--amqp-host=HOST``    RabbitMQ host [host]
  ``-i[FILE]``       ``--pipelines[=FILE]``  read request pipelines from external file [config. file]
  ``-s PORT``        ``--port=PORT``         server port
  ``-l[FILE]``       ``--access-log[=FILE]`` enable logging to file [log file]
  ..                 ``--colors``            use colors in log output
  ..                 ``--size=SIZE``         log file size
  ``-h HOST``        ``--db-host=HOST``      database host
  ``-d DB``          ``--db-name=DB``        database name
  ``-u USER``        ``--db-user=USER``      database user
  ``-p PASS``        ``--db-password=PASS``  database password
  ``-P PORT``        ``--db-port=PORT``      database port
  ``-r FILE``        ``--routes-file=FILE``  route pattern configuration file
  ``-t``             ``--trust-localhost``   bypass HMAC authentication for requests from localhost
  ..                 ``--pool-size=SIZE``    number of connections to keep in PostgreSQL connection pool
  ..                 ``--verbose``           print various debug information to stdout
==================== ======================= ==============================================================


Defaults
--------

Many of these settings have sensible default values.

=============== ===========
Option          Value
=============== ===========
AMQP user	"guest"
AMQP password	"guest"
Server port	3010
Log file	"log/access.log"
Log size	4,096 bytes
DB-host	        "localhost"
DB-name	        "trombone"
DB-user	        "postgres"
DB-password	"postgres"
DB-port	        5432
Pipelines file	"pipelines.conf"
Pool size	10
=============== ===========

