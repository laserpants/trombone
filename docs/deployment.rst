Deployment
==========

Heroku
------

Trombone has been successfully deployed to Heroku, although the process described here should be considered somehow experimental.

We will use `Joe Nelson's (begriffs) excellent buildpack <https://github.com/begriffs/heroku-buildpack-ghc>`_ to deploy to Heroku, using git. Various other Haskell buildpacks are available, but none of these have been tested with Trombone.

We assume you have your app running locally and the Heroku Toolbelt installed on your local machine. The first step, unless you have an existing git repository, is to initialize (``git init``) one and add the project files.

Then create the Heroku app using the ``heroku-buildpack-ghc`` buildpack.

::

    heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git


Log in to Heroku's web panel and add a PostgreSQL database service to your application. 

Now add a ``Procfile`` to your project. The file should be in the root directory (along with ``Main.hs`` etc.).

Procfile template
`````````````````

::

    web: dist/build/trombone/trombone -C -s $PORT -h <hostname> -d <database> --db-user=<db-user> --db-password=<db-password> -P 5432


Insert ``<hostname>`` and other details. The hostname is typically of the format ``xxxxxxxxx.amazonaws.com``. These particulars are available from the Heroku web admin after creating the database.

::

    git add Procfile
    git commit -m 'Added Procfile for Heroku.'


We can now go ahead and push to heroku.

::

    git push heroku master


Note that if you are using a local branch other than master, you should use the command ``git push heroku localbranch:master``.

15 minutes of Fail
`````````````````

The build will most likely fail due to Heroku's 15-minute time limit. Follow the `instructions here <https://github.com/begriffs/heroku-buildpack-ghc#beating-the-fifteen-minute-build-limit>`_.

@todo

@todo

@todo

