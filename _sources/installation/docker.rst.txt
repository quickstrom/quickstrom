Installing with Docker
======================

QuickStrom provides a Docker image as an easy installation method. Download
the image using Docker:

.. code-block:: console

   $ docker pull quickstrom/quickstrom:latest

As described in :doc:`../running`, Geckodriver must be running and available
on ``127.0.0.0:4444`` for Quickstrom to work. Download a Geckodriver image
using as well:

.. code-block:: console

   $ docker pull instrumentisto/geckodriver

You can now run Geckodriver and the ``quickstrom`` executable with ``docker
run``:

.. code-block:: console
   :linenos:

   $ docker run -d -p 4444:4444 instrumentisto/geckodriver
   $ docker run \
     --network=host \
     --mount=type=bind,source=$PWD/specs,target=/specs \
     quickstrom/quickstrom:latest \
     quickstrom check \
     /specs/Example.spec.purs \
     https://example.com

There's a lot of things going in the above session. Let's look at what each
line does:

1. Launch a geckodriver instance in a separate *detached* container
2. Uses `docker run` to execute a program inside the container
3. Uses `host network <https://docs.docker.com/network/host/>`__ to get easy access to Geckodriver (see below)
4. Mounts a host directory containing specification(s) to ``/specs`` in the container filesystem
5. Uses the image ``quickstrom/quickstrom`` with the ``latest`` target
6. Runs ``quickstrom`` with the ``check`` command
7. Passes a path to a specification file in the mounted directory
8. Passes an origin URI (this could also be a file path in the mounted directory)

There are `other ways
<https://docs.docker.com/engine/reference/run/#network-settings>`__ of
setting up network access between Docker containers. Using host networking is
convenient in this case, but you might require or prefer another method.