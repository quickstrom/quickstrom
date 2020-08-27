Running
=======

This guide assumes you've installed Quickstrom and geckodriver, as described
in one of the :doc:`installation guides <installation>`. If you haven't done
that already, start there.

To check a specification, you must have a running `geckodriver
<https://github.com/mozilla/geckodriver>`__ instance on ``127.0.0.0:4444``
(the default). Run the following command in a separate terminal or tab:

.. code-block:: console

   $ geckodriver

.. note::

   The invocation of geckodriver might look different depending on which
   installation method you've used.

Next, run ``quickstrom check`` and supply the path to the specification
file along with the origin URL (can also be a local file path).

.. code-block:: console

   $ quickstrom check /path/to/my/specification http://example.com

No WebDriver Session
--------------------

If you get the following error:

.. code-block:: console

   $ quickstrom: user error (E NoSession)

It’s probably because the WebDriver package in Quickstrom failed to
clean up its session. This is a known bug. To work around it, restart
Geckodriver and rerun your Quickstrom command.

Help
----

There are various flags and options for the ``quickstrom`` executable.
Run with ``--help`` to learn more.
