Installing with Docker
======================

QuickStrom provides a Docker image as an easy installation method. Download
the image using Docker:

.. code-block:: console

   $ docker pull quickstrom/quickstrom:latest

Verify that Quickstrom can now be run using Docker:

.. code-block:: console

   $ docker run quickstrom/quickstrom:latest quickstrom version

You can now run Quickstrom with ``docker run``:

.. code-block:: console
   :linenos:

   $ docker run \
     --mount=type=bind,source=$PWD/specs,target=/specs \
     quickstrom/quickstrom:latest \
     quickstrom -I/specs \
     check example \
     https://example.com

There's a lot of things going in the above session. Let's look at what each
line does:

#. Uses `docker run` to execute a program inside the container
#. Mounts a host directory containing specification(s) to ``/specs`` in the container filesystem
#. Uses the image ``quickstrom/quickstrom`` with the ``latest`` target
#. Runs ``quickstrom`` with the mounted ``/specs`` directory as an include path
#. Checks the ``example`` specificiation module (i.e. ``/specs/example.strom``)
#. Passes an origin URI (this could also be a file path in the mounted directory)

Accessing a Server on the Host
==============================

If you wish to run Quickstrom in Docker and test a website being
hosted by the Docker host system you can set the url to ``localhost``
(or ``host.docker.internal`` for MacOS).


.. code-block:: console
   :linenos:

   $ docker run \
     --network=host \
     --mount=type=bind,source=$PWD/specs,target=/specs \
     quickstrom/quickstrom:latest \
     quickstrom -I/specs \
     check example \
     http://localhost:3000 # or http://host.docker.internal:3000 for MacOS

You may have to disable HOST checking if you get "Invalid Host header" messages.
